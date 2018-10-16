#include <stdio.h>
#include <math.h>
#include <gu/string.h>
#include <gu/mem.h>
#include <gu/seq.h>
#include <gu/map.h>
#include <gu/string.h>
#include <gu/ucs.h>
#include <gu/utf8.h>
#include <pgf/pgf.h>

// #define TESTING

typedef struct {
	prob_t prob;
	prob_t count;
} ProbCount;

typedef struct {
	PgfCId fun;
	ProbCount pc;
	GuMap* mods;
} FunStats;

typedef struct {
	FunStats* stats;
	prob_t prob;
	ProbCount **prob_counts;
} SenseChoice;

typedef struct DepTree {
	size_t index;
	GuBuf* choices;
	size_t n_children;
	struct DepTree* child[0];
} DepTree;

typedef int (*EMRankingCallback)(DepTree* dtree,
                                 SenseChoice* choice, PgfCId cat);

typedef struct {
	GuPool* pool;
	PgfPGF *pgf;
	GuBuf* dtrees;
	GuBuf* fields;
	GuMap* stats;
	GuBuf* root_choices;
	prob_t bigram_total;
	prob_t unigram_total;
	prob_t unigram_smoothing;
	bool break_trees;
	GuMap* callbacks;
} EMState;


EMState*
em_new_state(char* fpath)
{
	GuPool* pool = gu_new_pool();

	EMState* state = gu_new(EMState, pool);
	state->pool = pool;
	state->dtrees = gu_new_buf(DepTree*, pool);
	state->fields = NULL;
	state->stats  = gu_new_string_map(FunStats*, NULL, pool);
	state->bigram_total = 0;
	state->unigram_total = 0;
	state->unigram_smoothing = INFINITY;
	state->root_choices = gu_new_buf(SenseChoice, pool);
	state->break_trees = 0;
	state->callbacks = gu_new_string_map(EMRankingCallback, &gu_null_struct, pool);

	GuExn* err = gu_new_exn(pool);
	state->pgf = pgf_read(fpath, state->pool, err);
	if (state->pgf == NULL) {
		gu_pool_free(state->pool);
		return NULL;
	}

	return state;
}

void
em_free_state(EMState* state)
{
	gu_pool_free(state->pool);
}



#define CONLL_NUM_FIELDS 10
typedef GuString CONLLFields[CONLL_NUM_FIELDS];

typedef struct {
	PgfMorphoCallback base;
	EMState* state;
	GuBuf* parent_choices;
	GuBuf* choices;
} LookupCallback;

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

typedef struct {
	GuMapItor clo;
	EMState *state;
} SmoothingItor;

static void
smoothing_iter(GuMapItor* clo, const void* key, void* value, GuExn* err)
{
	SmoothingItor *self = gu_container(clo, SmoothingItor, clo);

	PgfCId fun = (PgfCId) key;

	FunStats **stats =
		gu_map_insert(self->state->stats, fun);
	if (*stats == NULL) {
		*stats = gu_new(FunStats, self->state->pool);
		(*stats)->fun = fun;
		(*stats)->pc.prob  = 0;
		(*stats)->pc.count = self->state->unigram_smoothing;
		(*stats)->mods =
			gu_new_string_map(ProbCount*, NULL, self->state->pool);
	}
	self->state->unigram_total += exp(-self->state->unigram_smoothing);
}

void
em_setup_preserve_trees(EMState *state)
{
	state->fields = gu_new_buf(GuBuf*, state->pool);
}

void
em_setup_unigram_smoothing(EMState *state, prob_t count)
{
	state->unigram_smoothing = -log(count);

	SmoothingItor itor;
	itor.clo.fn  = smoothing_iter;
	itor.state = state;
	pgf_iter_functions(state->pgf, &itor.clo, NULL);
}

static void
add_choice(LookupCallback* self, PgfCId lemma)
{
	SenseChoice* choice = gu_buf_extend(self->choices);
	choice->prob = 0;

	FunStats** stats =
		gu_map_insert(self->state->stats, lemma);
	if (*stats == NULL) {
		*stats = gu_new(FunStats, self->state->pool);
		(*stats)->fun = lemma;
		(*stats)->pc.prob  = 0;
		(*stats)->pc.count = INFINITY;
		(*stats)->mods =
			gu_new_string_map(ProbCount*, NULL, self->state->pool);
	}
	choice->stats = *stats;

	size_t n_parent_choices =
		gu_buf_length(self->parent_choices);
	choice->prob_counts =
		gu_new_n(ProbCount*, n_parent_choices, self->state->pool);
	for (size_t i = 0; i < n_parent_choices; i++) {
		SenseChoice* parent_choice =
			gu_buf_index(self->parent_choices, SenseChoice, i);

		ProbCount** pc =
			gu_map_insert(parent_choice->stats->mods, lemma);
		if (*pc == NULL) {
			*pc = gu_new(ProbCount, self->state->pool);
			(*pc)->prob  = 0;
			(*pc)->count = INFINITY;
		}

		choice->prob_counts[i] = *pc;
	}
}

static void
lookup_callback(PgfMorphoCallback* callback,
	            PgfCId lemma, GuString analysis, prob_t prob,
	            GuExn* err)
{
	LookupCallback* self =
		gu_container(callback, LookupCallback, base);

	bool found = false;
	for (int i = 0; i < gu_buf_length(self->choices); i++) {
		SenseChoice* choice = gu_buf_index(self->choices, SenseChoice, i);
		if (strcmp(choice->stats->fun,lemma) == 0) {
			found = true;
			break;
		}
	}

	if (!found)
		add_choice(self, lemma);
}

static void
init_counts(DepTree* dtree, GuBuf* parent_choices)
{
	size_t n_choices = gu_buf_length(dtree->choices);
	size_t n_parent_choices = gu_buf_length(parent_choices);

	prob_t p1 = log(n_choices);
	prob_t p2 = p1 + log(n_parent_choices);

	for (size_t i = 0; i < n_choices; i++) {
		SenseChoice* choice =
			gu_buf_index(dtree->choices, SenseChoice, i);

		choice->prob = 0;
		choice->stats->pc.count =
			log_add(choice->stats->pc.count,p1);

		for (int j = 0; j < n_parent_choices; j++) {
			choice->prob_counts[j]->count =
				log_add(choice->prob_counts[j]->count, p2);
		}
	}
}

static void
filter_dep_tree(EMState* state, DepTree* dtree, GuBuf* buf, GuBuf* parent_choices)
{
	CONLLFields* fields = gu_buf_get(buf, CONLLFields, dtree->index);

	EMRankingCallback callback =
		gu_map_get(state->callbacks, (*fields)[3], EMRankingCallback);
	if (callback != NULL) {
		size_t n_choices = gu_buf_length(dtree->choices);

		prob_t max = 0;
		for (size_t i = 0; i < n_choices; i++) {
			SenseChoice* choice =
				gu_buf_index(dtree->choices, SenseChoice, i);
			PgfType* ty =
				pgf_function_type(state->pgf, choice->stats->fun);
			choice->prob = callback(dtree, choice, ty->cid);
			if (choice->prob > max) {
				max = choice->prob;
			}
		}

		size_t index = 0;
		for (size_t i = 0; i < n_choices; i++) {
			SenseChoice* choice =
				gu_buf_index(dtree->choices, SenseChoice, i);
			if (choice->prob == max) {
				SenseChoice* dest =
					gu_buf_index(dtree->choices, SenseChoice, index);
				*dest = *choice;
				index++;
			}
		}
		gu_buf_trim_n(dtree->choices, n_choices - index);
	}

	init_counts(dtree, parent_choices);

	for (size_t i = 0; i < dtree->n_children; i++) {
		filter_dep_tree(state, dtree->child[i], buf, dtree->choices);
	}
}

static DepTree*
build_dep_tree(EMState* state, PgfConcr* concr,
               GuBuf* buf, size_t index, CONLLFields* fields,
               GuBuf* parent_choices)
{
	GuString id = (*fields)[0];

	// first count how many children we have
	size_t n_children = 0;
	for (int i = 0; i < gu_buf_length(buf); i++) {
		CONLLFields* fields = gu_buf_get(buf, CONLLFields, i);
		if (strcmp((*fields)[6],id) == 0) {
			n_children++;
		}
	}

	DepTree* dtree = gu_new_flex(state->pool, DepTree, child, n_children);
	dtree->index      = index;
	dtree->choices    = gu_new_buf(SenseChoice, state->pool);
	dtree->n_children = n_children;

	LookupCallback callback;
	callback.base.callback = lookup_callback;
	callback.state = state;
	callback.parent_choices = parent_choices;
	callback.choices = dtree->choices;

	pgf_lookup_morpho(concr, (*fields)[1], &callback.base, NULL);
	if (gu_buf_length(dtree->choices) == 0) {
		// try with lower case
		char buffer[strlen((*fields)[1])*6+1];

		const uint8_t* src = (uint8_t*) (*fields)[1];
		uint8_t* dst = (uint8_t*) buffer;

		while (*src) {
			GuUCS ucs = gu_utf8_decode(&src);
			ucs = gu_ucs_to_lower(ucs);
			gu_utf8_encode(ucs, &dst);
		}
		*(dst++) = 0;

		pgf_lookup_morpho(concr, buffer, &callback.base, NULL);
	}

	if (state->fields != NULL || gu_buf_length(dtree->choices) > 0) {
		state->unigram_total++;

		// now build the children
		int index = 0;
		for (int i = 0; i < gu_buf_length(buf); i++) {
			CONLLFields* fields = gu_buf_get(buf, CONLLFields, i);
			if (strcmp((*fields)[6],id) == 0) {
				dtree->child[index] = 
					build_dep_tree(state, concr,
					               buf, i, fields,
								   dtree->choices);
				if (dtree->child[index] != NULL) {
					index++;
					state->bigram_total++;
				} else {
					dtree->n_children--;  // found child with out-of-voc word.
				}
			}
		}
		return dtree;
	} else {
		// the root has out-of-voc word.

		for (int i = 0; i < gu_buf_length(buf); i++) {
			CONLLFields* fields = gu_buf_get(buf, CONLLFields, i);
			if (strcmp((*fields)[6],id) == 0) {
				DepTree* child = 
					build_dep_tree(state, concr,
					               buf, i, fields,
								   state->root_choices);
				if (child != NULL) {
					filter_dep_tree(state, child, buf, state->root_choices);
					gu_buf_push(state->dtrees, DepTree*, child);
				}
			}
		}

		return NULL;
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

	FILE* inp = fopen(fpath, "r");
	if (!inp) {
		fprintf(stderr, "Error opening %s\n", fpath);
		return 0;
	}

	GuPool* tmp_pool;
	if (state->fields == NULL)
		tmp_pool = gu_new_pool();
	else
		tmp_pool = state->pool;
	GuBuf* buf = gu_new_buf(CONLLFields, tmp_pool);

	char line[2048];
	while (fgets(line, sizeof(line), inp)) {
		int len = strlen(line);
		if (len < 1 || line[len-1] != '\n') {
			fprintf(stderr, "Error in reading. Last read: %s\n", line);
			return 0;
		}

		// skip comments
		if (line[0] == '#')
			continue;

		// empty line signals the end of a sentence
		if (line[0] == '\n') {
			DepTree *dtree = NULL;
			for (int i = 0; i < gu_buf_length(buf); i++) {
				CONLLFields* fields = gu_buf_get(buf, CONLLFields, i);
				if (strcmp((*fields)[6], "0") == 0) {
					dtree = build_dep_tree(state, concr,
					                       buf, i, fields,
					                       state->root_choices);
					break;
				}
			}

			if (dtree != NULL) {
				filter_dep_tree(state, dtree, buf, state->root_choices);
				gu_buf_push(state->dtrees, DepTree*, dtree);
			}

			if (state->fields == NULL) {
				// if we don't need to save the CoNLL fields,
				// release the current pool and create a new one
				gu_pool_free(tmp_pool);
				tmp_pool = gu_new_pool();
			} else {
				// add the fields to the state
				gu_buf_push(state->fields, GuBuf*, buf);
			}
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
				return 0;
			}

			(*fields)[n_fields++] = field;

			if (*end == '\n')
				break;

			end++;
			start = end;
		}

		if (n_fields != CONLL_NUM_FIELDS) {
			fprintf(stderr, "Too few fields in: %s\n", line);
			return 0;
		}
    }

	if (state->fields == NULL) {
		gu_pool_free(tmp_pool);
	}

	fclose(inp);
	return 1;
}

static GuString
get_field(CONLLFields* fields, size_t i)
{
	return (fields ? (*fields)[i] : "_");
}

static void
print_conll_tree(FILE* out, DepTree* dtree, DepTree* parent, GuBuf* buf)
{
	size_t i = 0;
	while (i < dtree->n_children &&
	       dtree->child[i]->index < dtree->index) {
		print_conll_tree(out, dtree->child[i], dtree, buf);
		i++;
	}

	CONLLFields* fields =
		buf ? gu_buf_index(buf, CONLLFields, dtree->index) : NULL;

	fprintf(out, "%ld\t%s\t%s\t%s\t%s\t%s\t%ld%s\t%s\t%s\t%s\t",
	             dtree->index,
	             get_field(fields,1),
	             get_field(fields,2),
	             get_field(fields,3),
	             get_field(fields,4),
	             get_field(fields,5),
	             (parent ? parent->index : 0),
	             get_field(fields,7),
	             get_field(fields,8),
	             get_field(fields,9),
	             get_field(fields,9));

	size_t n_choices = gu_buf_length(dtree->choices);
	for (size_t i = 0; i < n_choices; i++) {
		SenseChoice* choice =
			gu_buf_index(dtree->choices, SenseChoice, i);
		if (i > 0)
			fputc(' ', out);
		fputs(choice->stats->fun, out);
	}

	fputc('\n', out);

	while (i < dtree->n_children) {
		print_conll_tree(out, dtree->child[i], dtree, buf);
		i++;
	}
}

int
em_export_annotated_treebank(EMState* state, GuString fpath)
{
	FILE *out;
	if (fpath != NULL) {
		out = fopen(fpath, "w+");
		if (out == NULL) {
			return 0;
		}
	} else {
		out = stdout;
	}

	size_t n_trees = gu_buf_length(state->dtrees);
	for (size_t i = 0; i < n_trees; i++) {
		DepTree* dtree = gu_buf_get(state->dtrees, DepTree*, i);
		GuBuf* buf = state->fields ? gu_buf_get(state->fields, GuBuf*, i) : NULL;
		print_conll_tree(out, dtree, NULL, buf);
		fprintf(out, "\n");
	}

	if (out != stdout)
		fclose(out);

	return 1;
}

int
em_unigram_count(EMState* state)
{
	return state->unigram_total;
}

int
em_bigram_count(EMState* state)
{
	return state->bigram_total;
}

void
em_set_ranking_callback(EMState* state,
                        GuString pos,
                        EMRankingCallback *ranking_callback)
{
	pos = gu_string_copy(pos, state->pool);
	gu_map_put(state->callbacks, pos, EMRankingCallback, ranking_callback);
}

static prob_t
tree_edge_estimation(size_t head_i, DepTree* mod)
{
	size_t n_choices = gu_buf_length(mod->choices);
	prob_t edge_prob = INFINITY;
	for (size_t i = 0; i < n_choices; i++) {
		SenseChoice* mod_choice =
			gu_buf_index(mod->choices, SenseChoice, i);
		edge_prob =
		   log_add(edge_prob,
				   mod_choice->prob_counts[head_i]->prob +
				   mod_choice->prob);
	}
	return edge_prob;
}

static void
tree_estimation(DepTree* dtree)
{
	for (size_t i = 0; i < dtree->n_children; i++) {
		tree_estimation(dtree->child[i]);
	}

	size_t n_choices = gu_buf_length(dtree->choices);
	for (size_t i = 0; i < n_choices; i++) {
		SenseChoice* choice =
			gu_buf_index(dtree->choices, SenseChoice, i);

		prob_t prob = 0;
		for (size_t j = 0; j < dtree->n_children; j++) {
			prob += tree_edge_estimation(i, dtree->child[j]);
		}

		choice->prob = prob;
	}
}

static void
tree_counting(EMState* state, DepTree* dtree, prob_t* outside_probs)
{
	size_t n_head_choices = gu_buf_length(dtree->choices);
	for (size_t j = 0; j < n_head_choices; j++) {
		SenseChoice* head_choice =
			gu_buf_index(dtree->choices, SenseChoice, j);

		prob_t prob = outside_probs[j] + head_choice->prob;
		head_choice->stats->pc.count = 
			log_add(head_choice->stats->pc.count, prob);
	}

	for (size_t i = 0; i < dtree->n_children; i++) {
		size_t n_child_choices =
			gu_buf_length(dtree->child[i]->choices);
		prob_t child_outside_probs[n_child_choices];
		for (size_t k = 0; k < n_child_choices; k++) {
			child_outside_probs[k] = INFINITY;
		}

		for (size_t j = 0; j < n_head_choices; j++) {
			SenseChoice* head_choice =
				gu_buf_index(dtree->choices, SenseChoice, j);

			if (head_choice->prob < INFINITY) {
				prob_t prob =
					outside_probs[j] + head_choice->prob -
					tree_edge_estimation(j, dtree->child[i]);

				for (size_t k = 0; k < n_child_choices; k++) {
					SenseChoice* mod_choice =
						gu_buf_index(dtree->child[i]->choices, SenseChoice, k);

					ProbCount* pc = mod_choice->prob_counts[j];

					prob_t p1 = prob + pc->prob;
					prob_t p2 = p1   + mod_choice->prob;
					child_outside_probs[k] = log_add(child_outside_probs[k],p1);
					pc->count = log_add(pc->count,p2);
				}
			}
		}

		tree_counting(state, dtree->child[i], child_outside_probs);
	}
}

#ifdef TESTING
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
 
typedef struct {
	GuMapItor clo1;
	GuMapItor clo2;
	EMState *state;
	FunStats* head_stats;
} NormIter;

static void
normalize_mods(GuMapItor* itor, const void* key, void* value, GuExn* err)
{
	NormIter* self = gu_container(itor, NormIter, clo2);
	PgfCId mod = key;
	ProbCount* pc = *((ProbCount**) value);
	pc->prob  = pc->count+log(self->state->bigram_total);
	pc->count = INFINITY;
}

static void
normalize_heads(GuMapItor* itor, const void* key, void* value, GuExn* err)
{
	NormIter* self = gu_container(itor, NormIter, clo1);
	FunStats* head_stats = *((FunStats**) value);

	self->head_stats = head_stats;

	head_stats->pc.prob  = head_stats->pc.count+log(self->state->unigram_total);
	head_stats->pc.count = self->state->unigram_smoothing;

	PgfType* ty = 
		pgf_function_type(self->state->pgf, self->head_stats->fun);

	gu_map_iter(self->head_stats->mods, &self->clo2, err);
}

prob_t
em_step(EMState *state)
{
	NormIter itor;
	itor.state = state;
	itor.clo1.fn = normalize_heads;
	itor.clo2.fn = normalize_mods;
	gu_map_iter(state->stats, &itor.clo1, NULL);

	prob_t corpus_prob = 0;
	for (int i = 0; i < gu_buf_length(state->dtrees); i++) {
		DepTree* dtree = gu_buf_get(state->dtrees, DepTree*, i);

		tree_estimation(dtree);

		prob_t sum = INFINITY;
		size_t n_choices = gu_buf_length(dtree->choices);
		for (size_t i = 0; i < n_choices; i++) {
			SenseChoice* choice =
				gu_buf_index(dtree->choices, SenseChoice, i);
			sum = log_add(sum, choice->prob);
		}

		corpus_prob += sum;

		prob_t outside_probs[n_choices];
		for (size_t j = 0; j < n_choices; j++) {
			outside_probs[j] = -sum;
		}

		tree_counting(state, dtree, outside_probs);
	}
	return corpus_prob;
}

typedef struct {
	GuMapItor clo1;
	GuMapItor clo2;
	EMState *state;
	GuMap *cat_probs;
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
	*pcount = log_add(*pcount, head_stats->pc.prob);
}

static void
dump_mods(GuMapItor* itor, const void* key, void* value, GuExn* err)
{
	DumpIter* self = gu_container(itor, DumpIter, clo2);
	PgfCId mod = key;

	ProbCount* pc = *((ProbCount**) value);
	
	double val = exp(-pc->prob);
	if (val > 1e-80)
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

	double val = exp(gu_map_get(self->cat_probs, ty->cid, prob_t)-head_stats->pc.prob);
	fprintf(self->funigram, "%s\t%e\n", head_stats->fun, val);

	gu_map_iter(self->head_stats->mods, &self->clo2, err);
}

static void
dump_cats(GuMapItor* itor, const void* key, void* value, GuExn* err)
{
	DumpIter* self = gu_container(itor, DumpIter, clo1);
	PgfCId cat  = (PgfCId) key;
	prob_t prob = *((prob_t*) value);

	fprintf(self->funigram, "%s\t%e\n", cat, exp(-prob));
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
