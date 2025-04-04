#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <math.h>
#include <pthread.h>
#include <gu/string.h>
#include <gu/mem.h>
#include <gu/seq.h>
#include <gu/map.h>
#include <gu/string.h>
#include <gu/ucs.h>
#include <gu/utf8.h>
#include <pgf/pgf.h>
#include "em_core.h"
#include "em_data_stream.h"
#include <time.h>

// #define DEBUG

#ifndef DISABLE_LZMA
#include <lzma.h>
#endif

typedef struct {
	EMState* state;
	size_t thread_idx;
	prob_t prob;
	size_t n_estimates;
	
	// Temporary buffers to keep the estimations for 
	// the inside probabilities. 
	//
	// The inside probability of a dtree when the root has
	// the j-th sense is inside_probs[dtree->index][j].
	// inside_probs[dtree->index] points into the estimates array.
	// The sizes of the two arrays are in state->max_tree_index and
	// state->max_tree_choices
	prob_t** inside_probs;
	prob_t*  estimates;
} EMThreadState;

struct EMState {
	GuPool* pool;
	GuExn* err;
	EMDataStream* stream;

	PgfPGF *pgf;
	GuMap* stats;
	size_t max_tree_index;
	size_t max_tree_choices;
	size_t bigram_total;
	size_t unigram_total;
	prob_t bigram_smoothing;
	prob_t unigram_smoothing;
	GuBuf* pcs;
	GuMap* callbacks;

	bool finished;
	size_t index1, index2;
	pthread_barrier_t barrier1, barrier2, barrier3;
	EMThreadState threads[NUM_THREADS];
};

#ifdef DEBUG
static void
print_tree(DepTree* dtree)
{
	printf("([");
	size_t n_choices = gu_buf_length(dtree->choices);
	for (size_t i = 0; i < n_choices; i++) {
		SenseChoice* choice =
			gu_buf_index(dtree->choices, SenseChoice, i);

		if (i > 0) printf(" ");
		printf("%e", choice->prob);
	}
	printf("]");
	for (size_t i = 0; i < dtree->n_children; i++) {
		putc(' ', stdout);
		print_tree(dtree->child[i]);
	}
	putc(')', stdout);
}
#endif

static void *
em_learner(void *arguments);

typedef struct {
	GuMapItor clo;
	EMState *state;
} FunctionItor;

static void
function_iter(GuMapItor* clo, const void* key, void* value, GuExn* err)
{
	FunctionItor *self = gu_container(clo, FunctionItor, clo);

	PgfCId fun = (PgfCId) key;

	FunStats **stats =
		gu_map_insert(self->state->stats, fun);
	if (*stats == NULL) {
		PgfType* ty = pgf_function_type(self->state->pgf, fun);

		*stats = gu_new(FunStats, self->state->pool);
		(*stats)->fun = fun;
		(*stats)->pc.prob  =
			pgf_category_prob(self->state->pgf, ty->cid) +
			pgf_function_prob(self->state->pgf, fun);

		(*stats)->mods =
			gu_new_string_map(ProbCount*, NULL, self->state->pool);

		for (size_t i = 0; i < NUM_THREADS; i++) {
			(*stats)->pc.count[i] = INFINITY;
		}

		if (gu_seq_length(ty->hypos) == 0)
			gu_buf_push(self->state->pcs, ProbCount*, &(*stats)->pc);

		self->state->unigram_total += exp(-self->state->unigram_smoothing);
	}	
}

EMState*
em_new_state(PgfPGF* pgf,
             prob_t unigram_smoothing, prob_t bigram_smoothing)
{
	GuPool* pool        = gu_new_pool();

	EMState* state = gu_new(EMState, pool);
	state->pool   = pool;
	state->err    = gu_new_exn(pool);
	state->stream = em_new_data_stream(64*1024*1024, 16*1024, NUM_THREADS, pool, state->err);
	if (gu_exn_is_raised(state->err)) {
		gu_pool_free(pool);
		return NULL;
	}
	state->stats  = gu_new_string_map(FunStats*, NULL, pool);
	state->max_tree_index = 0;
	state->max_tree_choices = 0;
	state->bigram_total = 0;
	state->unigram_total = 0;
	state->unigram_smoothing = -log(unigram_smoothing);
	state->bigram_smoothing  = -log(bigram_smoothing);
	state->pcs = gu_new_buf(ProbCount*, pool);

	state->callbacks = gu_new_string_map(EMRankingCallback, &gu_null_struct, pool);
	state->pgf = pgf;

	FunctionItor itor;
	itor.clo.fn  = function_iter;
	itor.state = state;
	pgf_iter_functions(state->pgf, &itor.clo, NULL);

	state->finished = false;
	state->index1 = 0;
	state->index2 = 0;

	if (pthread_barrier_init(&state->barrier1, NULL, NUM_THREADS+1) != 0) {
		em_data_stream_close(state->stream, state->err);
		gu_pool_free(pool);
		return NULL;
	}

	if (pthread_barrier_init(&state->barrier2, NULL, NUM_THREADS) != 0) {
		pthread_barrier_destroy(&state->barrier1);
		em_data_stream_close(state->stream, state->err);
		gu_pool_free(pool);
		return NULL;
	}

	if (pthread_barrier_init(&state->barrier3, NULL, NUM_THREADS+1) != 0) {
		pthread_barrier_destroy(&state->barrier1);
		pthread_barrier_destroy(&state->barrier2);
		em_data_stream_close(state->stream, state->err);
		gu_pool_free(pool);
		return NULL;
	}

	//create all learning threads one by one
	for (size_t i = 0; i < NUM_THREADS; i++) {
		pthread_t thread_id;

		state->threads[i].state = state;
		state->threads[i].thread_idx = i;
		state->threads[i].prob = 0;
		state->threads[i].inside_probs = NULL;
		state->threads[i].estimates = NULL;

		int result_code =
			pthread_create(&thread_id, NULL, em_learner,
			               &state->threads[i]);
		gu_assert(!result_code);

		char name[16];
		sprintf(name, "em_learner %ld", i);
		pthread_setname_np(thread_id, name);
	}

	return state;
}

void
em_free_state(EMState* state)
{
	state->finished = true;

	pthread_barrier_wait(&state->barrier1);

	pthread_barrier_destroy(&state->barrier1);
	pthread_barrier_destroy(&state->barrier2);
	pthread_barrier_destroy(&state->barrier3);
	em_data_stream_close(state->stream, state->err);
	gu_pool_free(state->pool);
}



#define CONLL_NUM_FIELDS 10
typedef struct {
	GuBuf* lemmas;
	GuString value[CONLL_NUM_FIELDS];
} CONLLFields;

typedef prob_t (*Oper)(prob_t x, prob_t y);

static prob_t
log_add(prob_t x, prob_t y)
{
	if (x == INFINITY)
		return y;
	if (y == INFINITY)
		return x;
	if (x < y)
		return x - log1p(exp(x - y));
	else
		return y - log1p(exp(y - x));
}

static prob_t
log_max(prob_t x, prob_t y)
{
	return ((x < y) ? x : y);
}

static prob_t
get_pgf_prob(EMState* state, PgfCId fun)
{
	PgfType* ty =
		pgf_function_type(state->pgf, fun);
	if (ty == NULL) {
		printf("Unknown function %s\n", fun);
		exit(1);
	}

	return pgf_category_prob(state->pgf, ty->cid) +
	       pgf_function_prob(state->pgf, fun);
}

static void
init_counts(EMState* state, DepTree* dtree,
            SenseChoice* parent_choices, size_t n_parent_choices,
            size_t *p_n_tree_choices)
{
	size_t n_choices = dtree->n_choices;

	prob_t p1 = log(n_choices);
	prob_t p2 = p1 + log(n_parent_choices);
	
	if (dtree->index > state->max_tree_index)
		state->max_tree_index = dtree->index;

	*p_n_tree_choices += n_choices;

	for (size_t i = 0; i < n_choices; i++) {
		SenseChoice* choice = &dtree->choices[i];

		choice->stats->pc.count[0] =
			log_add(choice->stats->pc.count[0],p1);

		choice->prob_counts =
			em_data_stream_malloc(state->stream, n_parent_choices*sizeof(ProbCount*));

		for (int j = 0; j < n_parent_choices; j++) {
			SenseChoice* parent_choice = &parent_choices[j];

			ProbCount** pc =
				gu_map_insert(parent_choice->stats->mods, choice->stats->fun);
			if (*pc == NULL) {
				prob_t back_off =
					get_pgf_prob(state,parent_choice->stats->fun) +
					get_pgf_prob(state,choice->stats->fun);

				*pc = gu_new(ProbCount, state->pool);
				(*pc)->prob  = state->bigram_smoothing + back_off;
				for (size_t i = 0; i < NUM_THREADS; i++) {
					(*pc)->count[i] = INFINITY;
				}
				gu_buf_push(state->pcs, ProbCount*, *pc);
			}

			choice->prob_counts[j] = *pc;

			choice->prob_counts[j]->count[0] =
				log_add(choice->prob_counts[j]->count[0], p2);
		}
	}
}

static void
filter_dep_tree(EMState* state, DepTree* dtree, GuSeq* conll,
                SenseChoice* parent_choices, size_t n_parent_choices,
                size_t *p_n_tree_choices)
{
	CONLLFields* fields = gu_seq_index(conll, CONLLFields, dtree->index);
	size_t n_lemmas = gu_buf_length(fields->lemmas);

	dtree->n_choices = 0;

	int max[2] = {INT_MIN, INT_MAX};
	int stats[n_lemmas][2];
	for (size_t i = 0; i < n_lemmas; i++) {
		PgfCId fun  = gu_buf_get(fields->lemmas, PgfCId, i);
		PgfType* ty = pgf_function_type(state->pgf, fun);

		EMRankingCallback callback =
			gu_map_get(state->callbacks, ty->cid, EMRankingCallback);
		if (callback != NULL) {
			callback(fun, conll, dtree, stats[i]);
		} else {
			stats[i][0] = 0;
			stats[i][1] = 0;
		}
		if (stats[i][0] > max[0]) {
			max[0] = stats[i][0];
			max[1] = stats[i][1];
			dtree->n_choices = 1;
		} else if (stats[i][0] == max[0]) {
			if (stats[i][1] < max[1]) {
				max[1] = stats[i][1];
				dtree->n_choices = 1;
			} else if (stats[i][1] == max[1]) {
				dtree->n_choices++;
			}
		}
	}

	dtree->choices =
		em_data_stream_malloc(state->stream, sizeof(SenseChoice)*dtree->n_choices);

	size_t index = 0;
	for (size_t i = 0; i < n_lemmas; i++) {
		if (stats[i][0] == max[0] && stats[i][1] == max[1]) {
			PgfCId lemma = gu_buf_get(fields->lemmas, PgfCId, i);

			SenseChoice* choice = &dtree->choices[index++];
			choice->prob_counts = NULL;
			choice->stats =
				gu_map_get(state->stats, lemma, FunStats*);
			gu_assert(choice->stats != NULL);
		}
	}

	init_counts(state, dtree, parent_choices, n_parent_choices, p_n_tree_choices);

	for (size_t i = 0; i < dtree->n_children; i++) {
		filter_dep_tree(state, dtree->children[i], conll,
		                dtree->choices, dtree->n_choices,
		                p_n_tree_choices);
	}
}

static DepTree*
build_dep_tree(EMState* state, PgfConcr* concr,
               GuSeq* conll, size_t index, CONLLFields* fields)
{
	GuString id = fields->value[0];

	// first count how many children we have
	size_t n_children = 0;
	for (int i = 0; i < gu_seq_length(conll); i++) {
		CONLLFields* fields = gu_seq_index(conll, CONLLFields, i);
		if (strcmp(fields->value[6],id) == 0) {
			n_children++;
		}
	}

	DepTree* dtree = em_data_stream_malloc(state->stream,
	                                       GU_FLEX_SIZE(DepTree, children, n_children));
	dtree->index      = index;
	dtree->n_children = n_children;
	dtree->n_choices  = 0;
	dtree->choices    = NULL;

	state->unigram_total++;

	// now build the children
	int pos = 0;
	for (int i = 0; i < gu_seq_length(conll); i++) {
		CONLLFields* fields = gu_seq_index(conll, CONLLFields, i);
		if (strcmp(fields->value[6],id) == 0) {
			dtree->children[pos++] = 
				build_dep_tree(state, concr,
							   conll, i, fields);
			state->bigram_total++;
		}
		}

	return dtree;
}

DepTree*
em_new_dep_tree(EMState* state, DepTree* parent,
                PgfCId fun, GuString lbl,
                size_t index, size_t n_children)
{
	DepTree* dtree = em_data_stream_malloc(state->stream,
	                                       GU_FLEX_SIZE(DepTree, children, n_children));
	dtree->index      = index;
	dtree->n_choices  = 1;
	dtree->choices    = em_data_stream_malloc(state->stream,sizeof(SenseChoice));
	dtree->n_children = n_children;

	if (dtree->index > state->max_tree_index)
		state->max_tree_index = dtree->index;
	if (dtree->index+1 > state->max_tree_choices)
		state->max_tree_choices = dtree->index+1;

	SenseChoice* choice = &dtree->choices[0];

	choice->stats =
		gu_map_get(state->stats, fun, FunStats*);
	assert(choice->stats != NULL);

	choice->stats->pc.count[0] =
		log_add(choice->stats->pc.count[0],0);

	choice->prob_counts = em_data_stream_malloc(state->stream,
	                                            sizeof(ProbCount*)*1);

	if (parent != NULL) {
		SenseChoice* parent_choice = &parent->choices[0];

		ProbCount** pc =
			gu_map_insert(parent_choice->stats->mods, choice->stats->fun);
		if (*pc == NULL) {
			prob_t back_off =
				get_pgf_prob(state,parent_choice->stats->fun) +
				get_pgf_prob(state,choice->stats->fun);

			*pc = gu_new(ProbCount, state->pool);
			(*pc)->prob  = state->bigram_smoothing + back_off;
			for (size_t i = 0; i < NUM_THREADS; i++) {
				(*pc)->count[i] = INFINITY;
			}
			gu_buf_push(state->pcs, ProbCount*, *pc);
		}

		choice->prob_counts[0] = *pc;

		choice->prob_counts[0]->count[0] =
			log_add(choice->prob_counts[0]->count[0], 0);
	}

	state->unigram_total++;
	state->bigram_total += n_children;

	return dtree;
}

DepTree*
em_new_conll_dep_tree(EMState* state, GuString lang, GuSeq* conll)
{
	PgfConcr* concr = pgf_get_language(state->pgf, lang);
	if (concr == NULL)
		return NULL;

	size_t n_tree_choices = 0;

	DepTree *dtree = NULL;
	for (int i = 0; i < gu_seq_length(conll); i++) {
		CONLLFields* fields = gu_seq_index(conll, CONLLFields, i);
		if (strcmp(fields->value[6], "0") == 0) {
			em_start_dep_tree(state);
			dtree = build_dep_tree(state, concr,
								   conll, i, fields);
			filter_dep_tree(state, dtree, conll,
			                NULL, 0, &n_tree_choices);
			break;
		}
	}

	if (state->max_tree_choices < n_tree_choices)
		state->max_tree_choices = n_tree_choices;

	return dtree;
}

void
em_start_dep_tree(EMState* state)
{
	em_data_stream_start_element(state->stream, state->err);
	if (gu_exn_is_raised(state->err)) {
		printf("em_start_dep_tree: i/o error\n");
		exit(1);
	}
}

void
em_add_dep_tree(EMState* state, DepTree* dtree)
{
	em_data_stream_add_element(state->stream, dtree);
}

void
em_increment_count(EMState* state, PgfCId fun)
{
	FunStats* stats =
		gu_map_get(state->stats, fun, FunStats*);
	assert (stats != NULL);
	stats->pc.prob = log_add(stats->pc.prob, 0);
}

#ifndef DISABLE_LZMA
static char*
lzma_fgets(char* inbuf, size_t insize, char* outbuf, size_t outsize, size_t* len,
           FILE *file, lzma_stream *stream, bool decompress)
{
	if (!decompress) {
		if (!fgets(outbuf, outsize, file))
			return NULL;
		*len = strlen(outbuf);
		return outbuf;
	}

	size_t index = 0;
	lzma_action action = LZMA_RUN;

	memmove(outbuf, outbuf+*len, (outsize-stream->avail_out)-*len);
	stream->next_out  -= *len;
	stream->avail_out += *len;

	while (true) {
		if (stream->avail_in == 0 && !feof(file)) {
			stream->next_in = inbuf;
			stream->avail_in =
				fread(inbuf, 1, insize, file);

			if (ferror(file)) {
				return NULL;
			}

			// Once the end of the input file has been reached,
			// we need to tell lzma_code() that no more input
			// will be coming. As said before, this isn't required
			// if the LZMA_CONATENATED flag isn't used when
			// initializing the decoder.
			if (feof(file))
				action = LZMA_FINISH;
		}

		lzma_ret ret = lzma_code(stream, action);
		if (ret != LZMA_OK) {
			return NULL;
		}

		size_t write_size = outsize - stream->avail_out;
		while (index < write_size) {
			if (outbuf[index++] == '\n') {
				*len = index;
				return outbuf;
			}
		}
		if (stream->avail_out == 0)
			return outbuf;
	}
}
#endif

typedef struct {
	PgfMorphoCallback base;
	EMState* state;
	GuBuf* lemmas;
} LookupCallback;

static void
lookup_callback(PgfMorphoCallback* callback,
	            PgfCId lemma, GuString analysis, prob_t prob,
	            GuExn* err)
{
	LookupCallback* self =
		gu_container(callback, LookupCallback, base);

	bool found = false;
	for (int i = 0; i < gu_buf_length(self->lemmas); i++) {
		if (strcmp(gu_buf_get(self->lemmas, PgfCId, i),lemma) == 0) {
			found = true;
			break;
		}
	}

	if (!found) {
		gu_buf_push(self->lemmas, PgfCId, lemma);
	}
}

int
em_import_treebank(EMState* state, GuString fpath, GuString lang)
{
	PgfConcr* concr = pgf_get_language(state->pgf, lang);
	if (concr == NULL) {
		fprintf(stderr, "Couldn't find language %s", lang);
		return 0;
	}

	FILE* inp;
#ifndef DISABLE_LZMA
	uint8_t inbuf[BUFSIZ];
	lzma_stream stream = LZMA_STREAM_INIT;
#endif
	char line[BUFSIZ];

	bool decompress = false;
	if (fpath == NULL || *fpath == 0)
		inp = stdin;
	else {
		inp = fopen(fpath, "r");

#ifndef DISABLE_LZMA
		if (inp != NULL && strcmp(fpath+(strlen(fpath)-3),".xz") == 0) {
			lzma_ret ret = lzma_stream_decoder(
								&stream, UINT64_MAX, LZMA_CONCATENATED);
			if (ret != LZMA_OK) {
				fprintf(stderr, "Error initializing LZMA %s\n", fpath);
				return 0;
			}
			stream.next_out  = line;
			stream.avail_out = sizeof(line);
			decompress = true;
		}
#endif
	}
	if (!inp) {
		fprintf(stderr, "Error opening %s\n", fpath);
		return 0;
	}

	GuPool* tmp_pool = gu_new_pool();
	GuBuf* buf = gu_new_buf(CONLLFields, tmp_pool);

#ifndef DISABLE_LZMA
	size_t len = 0;
	while (lzma_fgets(inbuf, sizeof(inbuf), line, sizeof(line), &len,
	                  inp, &stream, decompress)) {
#else
	while (fgets(line, sizeof(line), inp)) {
		size_t len = strlen(line);
#endif
		if (feof(inp)) {
			strcpy(line, "\n");
			len = 1;
		}

		if (len < 1 || line[len-1] != '\n') {
			fprintf(stderr, "Error in reading. Last read: %s\n", line);
			gu_pool_free(tmp_pool);
			fclose(inp);
			return 0;
		}

		// skip comments
		if (line[0] == '#')
			continue;

		// empty line signals the end of a sentence
		if (line[0] == '\n') {
			GuSeq* conll = gu_buf_data_seq(buf);
			for (int i = 0; i < gu_seq_length(conll); i++) {
				CONLLFields* fields = gu_seq_index(conll, CONLLFields, i);
				if (strcmp(fields->value[6], "0") == 0) {
					em_start_dep_tree(state);
					DepTree *dtree = 
						build_dep_tree(state, concr,
					                   conll, i, fields);
					size_t n_tree_choices = 0;
					filter_dep_tree(state, dtree, conll, 
					                NULL, 0, &n_tree_choices);
					if (state->max_tree_choices < n_tree_choices)
						state->max_tree_choices = n_tree_choices;
					em_add_dep_tree(state, dtree);
					break;
				}
			}

			gu_pool_free(tmp_pool);
			tmp_pool = gu_new_pool();
			buf = gu_new_buf(CONLLFields, tmp_pool);
			continue;
		}

		size_t n_fields = 0;
		CONLLFields* fields = gu_buf_extend(buf);
		char* start = line;
		char* end   = line;
		for (;;) {
			while (*end != '\n' && *end != '\t') {
				end++;
			}

			size_t len = end-start;
			char* field = gu_malloc(tmp_pool, len+1);
			memcpy(field, start, len);
			field[len] = 0;

			if (n_fields >= CONLL_NUM_FIELDS) {
				fprintf(stderr, "Too many fields in: %s\n", line);
				gu_pool_free(tmp_pool);
				fclose(inp);
				return 0;
			}

			fields->value[n_fields++] = field;

			if (*end == '\n') {
				break;
			}

			end++;
			start = end;
		}

		LookupCallback callback;
		callback.base.callback = lookup_callback;
		callback.state  = state;
		callback.lemmas = gu_new_buf(PgfCId, tmp_pool);

		pgf_lookup_morpho(concr, fields->value[1], &callback.base, NULL);
		if (gu_buf_length(callback.lemmas) == 0) {
			// try with lower case
			char buffer[strlen(fields->value[1])*6+1];

			const uint8_t* src = (uint8_t*) fields->value[1];
			uint8_t* dst = (uint8_t*) buffer;

			while (*src) {
				GuUCS ucs = gu_utf8_decode(&src);
				ucs = gu_ucs_to_lower(ucs);
				gu_utf8_encode(ucs, &dst);
			}
			*(dst++) = 0;

			pgf_lookup_morpho(concr, buffer, &callback.base, NULL);
		}

		fields->lemmas = callback.lemmas;

		while (n_fields < CONLL_NUM_FIELDS) {
			fields->value[n_fields++] = "";
		}
    }

	gu_pool_free(tmp_pool);

	fclose(inp);
#ifndef DISABLE_LZMA
	lzma_end(&stream);
#endif
	return 1;
}

int
em_load_model(EMState* state, GuString fpath)
{
	FILE* inp = fopen(fpath, "r");
	if (!inp) {
		fprintf(stderr, "Error opening %s\n", fpath);
		return 0;
	}

	char line[2048];
	while (fgets(line, sizeof(line), inp)) {
		int len = strlen(line);
		if (len < 1 || line[len-1] != '\n') {
			fprintf(stderr, "Error in reading. Last read: %s\n", line);
			fclose(inp);
			return 0;
		}

		size_t n_fields = 0;
		GuString fields[3];
		char* start = line;
		char* end   = line;
		for (;;) {
			while (*end != '\n' && *end != '\t') {
				end++;
			}

			if (n_fields >= 3) {
				fprintf(stderr, "Too many fields in: %s\n", line);
				fclose(inp);
				return 0;
			}

			char c = *end;
			*end = 0;
			fields[n_fields++] = start;

			if (c == '\n')
				break;

			end++;
			start = end;
		}

		if (n_fields != 3) {
			fprintf(stderr, "Too few fields in: %s\n", line);
			fclose(inp);
			return 0;
		}

		FunStats* stats = gu_map_get(state->stats, fields[0], FunStats*);
		fields[1] = *((GuString*) gu_map_find_key(state->stats, fields[1]));
		ProbCount** pc = gu_map_insert(stats->mods, fields[1]);
		if (*pc == NULL) {
			prob_t back_off =
				get_pgf_prob(state,fields[0]) +
				get_pgf_prob(state,fields[1]);

			prob_t bigram_smoothing1m = 
				-log1p(-exp(-state->bigram_smoothing));

			prob_t prob =
				log_add(state->bigram_smoothing + back_off,
                        bigram_smoothing1m + atof(fields[2]));

			*pc = gu_new(ProbCount, state->pool);
			(*pc)->prob  = prob;
			for (size_t i = 0; i < NUM_THREADS; i++) {
				(*pc)->count[i] = INFINITY;
			}
			gu_buf_push(state->pcs, ProbCount*, *pc);
		}
	}

	fclose(inp);
	return 1;
}

size_t
em_unigram_count(EMState* state)
{
	return state->unigram_total;
}

size_t
em_bigram_count(EMState* state)
{
	return state->bigram_total;
}

void
em_set_ranking_callback(EMState* state,
                        PgfCId cat,
                        EMRankingCallback *ranking_callback)
{
	cat = gu_string_copy(cat, state->pool);
	gu_map_put(state->callbacks, cat, EMRankingCallback*, ranking_callback);
}

int
dtree_match_label(GuSeq* conll, DepTree *dtree, GuString lbl)
{
	CONLLFields* fields =
		gu_seq_index(conll, CONLLFields, dtree->index);
			
	if (strcmp(fields->value[7], lbl) == 0)
		return 1;

	return 0;
}

int
dtree_match_pos(GuSeq* conll, DepTree *dtree, GuString pos)
{
	CONLLFields* fields =
		gu_seq_index(conll, CONLLFields, dtree->index);

	if (strcmp(fields->value[3], pos) == 0)
		return 1;

	return 0;
}

int
dtree_match_same_lemma(GuSeq* conll, DepTree *dtree, PgfCId lemma)
{
	CONLLFields* fields =
		gu_seq_index(conll, CONLLFields, dtree->index);
	for (size_t i = 0; i < gu_buf_length(fields->lemmas); i++) {
		if (strcmp(lemma, gu_buf_get(fields->lemmas, PgfCId, i)) == 0)
			return 1;
	}
	return 0;
}

static prob_t
tree_sum_estimation(EMThreadState* tstate, DepTree* dtree, Oper oper)
{
	size_t n_choices = dtree->n_choices;
	if (n_choices == 0) {
		prob_t prob = 0;
		for (size_t i = 0; i < dtree->n_children; i++) {
			prob += tree_sum_estimation(tstate, dtree->children[i], oper);
		}
		return prob;
	} else {
		prob_t  prob         = INFINITY;
		prob_t *inside_probs = tstate->inside_probs[dtree->index];
		for (size_t i = 0; i < n_choices; i++) {
			prob = oper(prob, inside_probs[i]);
		}
		return prob;
	}
}

static prob_t
tree_edge_estimation(EMThreadState* tstate, 
                     size_t head_i, DepTree* mod, Oper oper)
{
	prob_t edge_prob = INFINITY;

	size_t n_choices = mod->n_choices;
	if (n_choices == 0) {
		edge_prob = tree_sum_estimation(tstate, mod, oper);
		if (edge_prob == INFINITY)
			return 0;
	}

	prob_t *inside_probs = tstate->inside_probs[mod->index];
	for (size_t i = 0; i < n_choices; i++) {
		edge_prob =
		   oper(edge_prob,
				mod->choices[i].prob_counts[head_i]->prob +
				inside_probs[i]);
	}
	return edge_prob;
}

static void
tree_estimation(EMThreadState* tstate, DepTree* dtree, Oper oper)
{
	for (size_t i = 0; i < dtree->n_children; i++) {
		tree_estimation(tstate, dtree->children[i], oper);
	}

	gu_assert(dtree->index <= tstate->state->max_tree_index);

	size_t n_choices = dtree->n_choices;
	prob_t *inside_probs = &tstate->estimates[tstate->n_estimates];
	tstate->inside_probs[dtree->index] = inside_probs;
	tstate->n_estimates += n_choices;

	gu_assert(tstate->n_estimates <= tstate->state->max_tree_choices);

	for (size_t i = 0; i < n_choices; i++) {
		prob_t prob = 0;
		for (size_t j = 0; j < dtree->n_children; j++) {
			prob += tree_edge_estimation(tstate, i, dtree->children[j], oper);
		}

		inside_probs[i] = prob;
	}
}

static void
tree_counting(EMThreadState* tstate, DepTree* dtree, prob_t* outside_probs)
{
	EMState *state = tstate->state;

	size_t n_head_choices = dtree->n_choices;
	prob_t *inside_probs = tstate->inside_probs[dtree->index];
	for (size_t j = 0; j < n_head_choices; j++) {
		SenseChoice* head_choice = &dtree->choices[j];

		prob_t prob = outside_probs[j] + inside_probs[j];
		head_choice->stats->pc.count[tstate->thread_idx] =
			log_add(head_choice->stats->pc.count[tstate->thread_idx], prob);
	}

	for (size_t i = 0; i < dtree->n_children; i++) {
		size_t n_child_choices = dtree->children[i]->n_choices;
		prob_t child_outside_probs[n_child_choices];
		prob_t *child_inside_probs =
			tstate->inside_probs[dtree->children[i]->index];

		if (n_head_choices > 0) {
			for (size_t k = 0; k < n_child_choices; k++) {
				child_outside_probs[k] = INFINITY;
			}

			for (size_t j = 0; j < n_head_choices; j++) {
				if (inside_probs[j] < INFINITY) {
					prob_t prob =
						outside_probs[j] + inside_probs[j] -
						tree_edge_estimation(tstate, j, dtree->children[i], log_add);

					for (size_t k = 0; k < n_child_choices; k++) {
						SenseChoice* mod_choice =
							&dtree->children[i]->choices[k];

						ProbCount* pc = mod_choice->prob_counts[j];

						prob_t p1 = prob + pc->prob;
						prob_t p2 = p1   + child_inside_probs[k];
						child_outside_probs[k] = log_add(child_outside_probs[k],p1);
						pc->count[tstate->thread_idx] =
							log_add(pc->count[tstate->thread_idx],p2);
					}
				}
			}
		} else {
			prob_t sum = tree_sum_estimation(tstate, dtree->children[i], log_add);
			for (size_t k = 0; k < n_child_choices; k++) {
				child_outside_probs[k] = -sum;
			}
		}

		tree_counting(tstate, dtree->children[i], child_outside_probs);
	}
}

static void *
em_learner(void *arguments)
{
	EMThreadState* tstate = (EMThreadState *) arguments;
	EMState* state = tstate->state;

	for (;;) {
		pthread_barrier_wait(&state->barrier1);

		if (state->finished)
			break;

		if (tstate->inside_probs == NULL)
			tstate->inside_probs =
				gu_new_n(prob_t*, state->max_tree_index+1, state->pool);
		if (tstate->estimates == NULL)
			tstate->estimates =
				gu_new_n(prob_t,state->max_tree_choices+1, state->pool);

		// Normalize counts to probabilities
		size_t n_pcs = gu_buf_length(state->pcs);
		while (state->index1 < n_pcs) {
			size_t batch = 256;
			size_t start = __sync_fetch_and_add(&state->index1, batch);

			size_t end   = start + batch;
			if (end > n_pcs)
				end = n_pcs;

			for (size_t i = start; i < end; i++) {
				ProbCount* pc =
					gu_buf_get(state->pcs, ProbCount*, i);
				pc->prob  = INFINITY;
				for (size_t i = 0; i < NUM_THREADS; i++) {
					pc->prob     = log_add(pc->prob, pc->count[i]);
					pc->count[i] = INFINITY;
				}
			}
		}

		em_data_stream_restart(state->stream, tstate->thread_idx, state->err);
		if (gu_exn_is_raised(state->err)) {
			printf("em_learner: i/o error\n");
			exit(1);
		}

		pthread_barrier_wait(&state->barrier2);

		// Estimate the new counts
		tstate->prob = 0;
		for(;;) {
			DepTree* dtree =
				em_data_stream_fetch_element(state->stream, tstate->thread_idx);
			if (dtree == NULL)
				break;

			tstate->n_estimates = 0;
			tree_estimation(tstate, dtree, log_max);

			prob_t sum = tree_sum_estimation(tstate, dtree, log_add);
			
			tstate->prob += sum;

			prob_t outside_probs[dtree->n_choices];
			for (size_t j = 0; j < dtree->n_choices; j++) {
				outside_probs[j] = -sum;
			}

			tree_counting(tstate, dtree, outside_probs);
		}

		pthread_barrier_wait(&state->barrier3);
	}
	return NULL;
}

prob_t
em_step(EMState *state)
{
	state->index1 = 0;
	state->index2 = 0;

	pthread_barrier_wait(&state->barrier1);

	//wait for all threads to complete
	pthread_barrier_wait(&state->barrier3);

	prob_t corpus_prob = state->bigram_total*log(state->bigram_total);
	for (int i = 0; i < NUM_THREADS; i++) {
		corpus_prob += state->threads[i].prob;
	}

	// return the new corpus probability
	return corpus_prob;
}

typedef struct {
	GuMapItor clo1;
	GuMapItor clo2;
	EMState *state;
	GuMap *cat_probs;
	prob_t cat_total;
	FILE *funigram, *fbigram;
	FunStats* head_stats;
} DumpIter;

static void
collect_cat_probs(GuMapItor* itor, const void* key, void* value, GuExn* err)
{
	DumpIter* self = gu_container(itor, DumpIter, clo1);
	FunStats* head_stats = *((FunStats**) value);
	
	PgfType *ty =
		pgf_function_type(self->state->pgf, head_stats->fun);
	prob_t *pcount =
		gu_map_insert(self->cat_probs, ty->cid);
	prob_t prob = log_add(head_stats->pc.prob,self->state->unigram_smoothing);
	*pcount = log_add(*pcount, prob);
	self->cat_total = log_add(self->cat_total, prob);
}

static void
dump_mods(GuMapItor* itor, const void* key, void* value, GuExn* err)
{
	DumpIter* self = gu_container(itor, DumpIter, clo2);
	PgfCId mod = key;

	ProbCount* pc = *((ProbCount**) value);
	
	double val = exp(-pc->prob);
	if (val*self->state->bigram_total > 0.00001)
		fprintf(self->fbigram, "%s\t%s\t%e\n",
		                       self->head_stats->fun, mod, 
		                       val);
}

static void
dump_heads(GuMapItor* itor, const void* key, void* value, GuExn* err)
{
	DumpIter* self = gu_container(itor, DumpIter, clo1);
	FunStats* head_stats = *((FunStats**) value);

	self->head_stats = head_stats;

	PgfType *ty =
		pgf_function_type(self->state->pgf, head_stats->fun);

	double val = exp(gu_map_get(self->cat_probs, ty->cid, prob_t)-log_add(head_stats->pc.prob,self->state->unigram_smoothing));
	fprintf(self->funigram, "%s\t%e\n", head_stats->fun, val);

	gu_map_iter(self->head_stats->mods, &self->clo2, err);
}

static void
dump_cats(GuMapItor* itor, const void* key, void* value, GuExn* err)
{
	DumpIter* self = gu_container(itor, DumpIter, clo1);
	PgfCId cat  = (PgfCId) key;
	prob_t prob = *((prob_t*) value);

	fprintf(self->funigram, "%s\t%e\n", cat, exp(self->cat_total-prob));
}

void
em_dump(EMState *state, char* unigram_path, char* bigram_path)
{
	GuPool* tmp_pool = gu_local_pool();

	prob_t inf = INFINITY;

	DumpIter itor;
	itor.state = state;
	itor.cat_probs =
		gu_new_string_map(prob_t, &inf, tmp_pool);
	itor.cat_total = INFINITY;

	itor.clo1.fn = collect_cat_probs;
	gu_map_iter(state->stats, &itor.clo1, NULL);

	itor.funigram = fopen(unigram_path, "w+");	
	itor.fbigram = fopen(bigram_path, "w+");

	itor.clo1.fn = dump_heads;
	itor.clo2.fn = dump_mods;
	gu_map_iter(state->stats, &itor.clo1, NULL);

	itor.clo1.fn = dump_cats;
	gu_map_iter(itor.cat_probs, &itor.clo1, NULL);

	fclose(itor.fbigram);
	fclose(itor.funigram);

	gu_pool_free(tmp_pool);
}

typedef struct {
	PgfCId fun;
	prob_t prob;
} LemmaProb;

static int cmp_lemma_prob(const void *p1, const void *p2)
{
	LemmaProb *lp1 = (LemmaProb *) p1;
	LemmaProb *lp2 = (LemmaProb *) p2;
	if (lp1->prob < lp2->prob)
	  return -1;
	else if (lp1->prob > lp2->prob)
	  return 1;
	else
	  return 0;
}

static void
print_abstract_head(EMThreadState* tstate, FILE* out, DepTree* dtree,
                    prob_t* outside_probs)
{
	if (dtree->n_choices > 0) {
		LemmaProb choices[dtree->n_choices];
		prob_t *inside_probs = tstate->inside_probs[dtree->index];
		for (size_t j = 0; j < dtree->n_choices; j++) {
			SenseChoice* choice = &dtree->choices[j];
			choices[j].fun  = choice->stats->fun;
			choices[j].prob = outside_probs[j]+inside_probs[j];
		}
		qsort(choices, dtree->n_choices, sizeof(LemmaProb), cmp_lemma_prob);

		int first = 0;
		prob_t best_prob;
		if (dtree->n_choices > 1)
			fputc('[', out);
		for (size_t j = 0; j < dtree->n_choices; j++) {
			switch (first) {
			case 0:
				best_prob = choices[j].prob;
				first = 1;
				break;
			case 1:
				if (choices[j].prob > best_prob) {
					first = 2;
					fputs(" |", out);
				}
			case 2:
				fputc(' ', out);
			}
			fputs(choices[j].fun, out);
		}
		if (dtree->n_choices > 1)
			fputc(']', out);
	} else {
		fputs("[]", out);
	}
}

static void
print_abstract_tree(EMThreadState* tstate,
                    FILE* out, DepTree* dtree,
                    prob_t* outside_probs)
{
	size_t n_head_choices = dtree->n_choices;
	prob_t *inside_probs  = tstate->inside_probs[dtree->index];

	if (dtree->n_children > 0)
		fputc('(', out);

	print_abstract_head(tstate, out, dtree,
                        outside_probs);

	for (size_t i = 0; i < dtree->n_children; i++) {
		size_t n_child_choices = dtree->children[i]->n_choices;
		prob_t child_outside_probs[n_child_choices];

		if (n_head_choices > 0) {
			for (size_t k = 0; k < n_child_choices; k++) {
				child_outside_probs[k] = INFINITY;
			}

			for (size_t j = 0; j < n_head_choices; j++) {
				SenseChoice* head_choice = &dtree->choices[j];

				prob_t prob =
					outside_probs[j] + inside_probs[j] -
					tree_edge_estimation(tstate, j, dtree->children[i], log_max);

				for (size_t k = 0; k < n_child_choices; k++) {
					SenseChoice* mod_choice = &dtree->children[i]->choices[k];

					ProbCount* pc = mod_choice->prob_counts[j];

					prob_t p1 = prob + pc->prob;
					child_outside_probs[k] = log_max(child_outside_probs[k],p1);
				}
			}
		} else {
			prob_t sum = tree_sum_estimation(tstate, dtree->children[i], log_max);
			for (size_t k = 0; k < n_child_choices; k++) {
				child_outside_probs[k] = -sum;
			}
		}

		fputc(' ', out);
		print_abstract_tree(tstate, out, dtree->children[i],
		                    child_outside_probs);
	}

	if (dtree->n_children > 0)
		fputc(')', out);
}

int
em_export_abstract_treebank(EMState* state, GuString fpath)
{
	EMThreadState* tstate = &state->threads[0];

	FILE *out;
	if (fpath == NULL || *fpath == 0)
		out = stdout;
	else {
		out = fopen(fpath, "w+");
		if (out == NULL) {
			return 0;
		}
	}
	
	em_data_stream_restart(state->stream, tstate->thread_idx, state->err);
	if (gu_exn_is_raised(state->err)) {
		printf("em_export_abstract_treebank: i/o error\n");
		exit(1);
	}

	for (;;) {
		DepTree* dtree = 
			em_data_stream_fetch_element(state->stream, tstate->thread_idx);

		tstate->n_estimates = 0;
		tree_estimation(tstate, dtree, log_max);

		prob_t max = tree_sum_estimation(tstate, dtree, log_max);
		prob_t outside_probs[dtree->n_choices];
		for (size_t j = 0; j < dtree->n_choices; j++) {
			outside_probs[j] = -max;
		}

		print_abstract_tree(tstate, out, dtree, outside_probs);
		fprintf(out, "\n");
	}

	if (out != stdout)
		fclose(out);

	return 1;
}

static void
em_annotate_dep_tree_(EMThreadState* tstate,
                      GuBuf* buf, DepTree* dtree, prob_t* outside_probs)
{
	size_t n_head_choices = dtree->n_choices;
	prob_t *inside_probs = tstate->inside_probs[dtree->index];

	if (n_head_choices > 0) {
		EMLemmaProb* choices = gu_buf_extend_n(buf, n_head_choices);
		for (size_t j = 0; j < n_head_choices; j++) {
			SenseChoice* choice = &dtree->choices[j];
			choices[j].index= dtree->index;
			choices[j].fun  = choice->stats->fun;
			choices[j].prob = outside_probs[j]+inside_probs[j];
		}
		qsort(choices, n_head_choices, sizeof(EMLemmaProb), cmp_lemma_prob);
	}

	for (size_t i = 0; i < dtree->n_children; i++) {
		size_t n_child_choices = dtree->children[i]->n_choices;
		prob_t child_outside_probs[n_child_choices];

		if (n_head_choices > 0) {
			for (size_t k = 0; k < n_child_choices; k++) {
				child_outside_probs[k] = INFINITY;
			}

			for (size_t j = 0; j < n_head_choices; j++) {
				prob_t prob =
					outside_probs[j] + inside_probs[j] -
					tree_edge_estimation(tstate, j, dtree->children[i], log_max);

				for (size_t k = 0; k < n_child_choices; k++) {
					SenseChoice* mod_choice = &dtree->children[i]->choices[k];
					ProbCount* pc = mod_choice->prob_counts[j];

					prob_t p1 = prob + pc->prob;
					child_outside_probs[k] = log_max(child_outside_probs[k],p1);
				}
			}
		} else {
			prob_t sum = tree_sum_estimation(tstate, dtree->children[i], log_max);
			for (size_t k = 0; k < n_child_choices; k++) {
				child_outside_probs[k] = -sum;
			}
		}

		em_annotate_dep_tree_(tstate, buf, dtree->children[i], child_outside_probs);
	}
}

GuBuf*
em_annotate_dep_tree(EMState* state, DepTree* dtree, GuPool* pool)
{
	EMThreadState *tstate = &state->threads[0];
	tstate->n_estimates = 0;
	tree_estimation(tstate, dtree, log_max);

	prob_t max = tree_sum_estimation(tstate, dtree, log_max);
	prob_t outside_probs[dtree->n_choices];
	for (size_t j = 0; j < dtree->n_choices; j++) {
		outside_probs[j] = -max;
	}

	GuBuf* buf = gu_new_buf(EMLemmaProb, pool);
	em_annotate_dep_tree_(tstate, buf, dtree, outside_probs);
	return buf;
}


