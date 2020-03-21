#include <geanyplugin.h>
#include <unistd.h> /* read, write, close */
#include <string.h> /* memcpy, memset */
#include <sys/socket.h> /* socket, connect */
#include <netinet/in.h> /* struct sockaddr_in, struct sockaddr */
#include <netdb.h> /* struct hostent, gethostbyname */
#include <errno.h>
#include <stdlib.h>
#include "jsmn.h"


enum
{
	KB_LOOKUP_WORD,
	KB_GF_WORDNET
};


/* Function realloc_it() is a wrapper function for standard realloc()
 * with one difference - it frees old memory pointer in case of realloc
 * failure. Thus, DO NOT use old data pointer in anyway after call to
 * realloc_it(). If your code has some kind of fallback algorithm if
 * memory can't be re-allocated - use standard realloc() instead.
 */
static inline void *realloc_it(void *ptrmem, size_t size) {
	void *p = realloc(ptrmem, size);
	if (!p)  {
		free (ptrmem);
		fprintf(stderr, "realloc(): errno=%d\n", errno);
	}
	return p;
}

gboolean json_request(char** json, jsmntok_t **json_toks, size_t* json_size, gchar *host, gchar* url_fmt, ...)
{
	va_list a_list;
    va_start(a_list, url_fmt);

    /* first what are we going to send and where are we going to send it? */
    int portno =        80;

    struct hostent *server;
    struct sockaddr_in serv_addr;
    int sockfd, bytes, sent, total;
    char buffer[1024];

    /* fill in the parameters */
    strcpy(buffer,"GET ");
    vsnprintf(buffer+4,sizeof(buffer)-(27+strlen(host)),url_fmt,a_list);
    strcat(buffer," HTTP/1.1\r\nhost: ");
    strcat(buffer,host);
    strcat(buffer,"\r\n\r\n");

    /* create the socket */
    sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd < 0) return FALSE;

    /* lookup the ip address */
    server = gethostbyname(host);
    if (server == NULL) {
        close(sockfd);
        return FALSE;
	}

    /* fill in the structure */
    memset(&serv_addr,0,sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(portno);
    memcpy(&serv_addr.sin_addr.s_addr,server->h_addr,server->h_length);

    /* connect the socket */
    if (connect(sockfd,(struct sockaddr *)&serv_addr,sizeof(serv_addr)) < 0) {
        close(sockfd);
        return FALSE;
    }

    /* send the request */
    total = strlen(buffer);
    sent  = 0;
    do {
        bytes = write(sockfd,buffer+sent,total-sent);
        if (bytes < 0) {
            close(sockfd);
            return FALSE;
        }
        if (bytes == 0)
            break;
        sent+=bytes;
    } while (sent < total);

	char *js = NULL;
	size_t jslen = 0;
	gboolean skip_headers = TRUE;
	size_t content_length = 0;

	do {
        /* Read another chunk */
        bytes = read(sockfd,buffer,sizeof(buffer));
        if (bytes < 0) {
            close(sockfd);
            free(js);
            return FALSE;
        }
        if (bytes == 0)
			break;

		js = realloc_it(js, jslen + bytes + 1);
		if (js == NULL) {
            close(sockfd);
            return FALSE;
		}
		strncpy(js + jslen, buffer, bytes);
		jslen = jslen + bytes;

        if (skip_headers) {
            char* s = js+4;
            while (s <= js+jslen) {
                if (s[-4] == '\r' && s[-3] == '\n' &&
                    s[-2] == '\r' && s[-1] == '\n') {
                    skip_headers = FALSE;
                    break;
				}
				s++;
			}
			
			if (!skip_headers) {
				// we got all the headers, now find the content length
				char* header = "Content-Length:";
				size_t len = strlen(header);

				char* s1 = js;
				while (s1 <= s) {
					if (strncmp(s1,header,len) == 0) {
						s1 += len;
						while (s1 <= s && *s1 == ' ')
							s1++;
						content_length = atoi(s1);
						break;
					}
					s1++;
				}

				jslen -= s-js;
				memmove(js, s, jslen);
			}
        }
	} while (skip_headers || jslen < content_length);

	jsmn_parser p;
	jsmntok_t *tok;
	size_t tokcount = 128;

	/* Prepare parser */
	jsmn_init(&p);

	/* Allocate some tokens as a start */
	tok = malloc(sizeof(*tok) * tokcount);
	if (tok == NULL) {
		free(js);
        close(sockfd);
        return FALSE;
    }

again:;
	int r = jsmn_parse(&p, js, jslen, tok, tokcount);
	if (r < 0) {
		if (r == JSMN_ERROR_NOMEM) {
			tokcount = tokcount * 2;
			tok = realloc_it(tok, sizeof(*tok) * tokcount);
			if (tok != NULL)
				goto again;
		}
         
        close(sockfd);
        free(tok);
        free(js);
        return FALSE;
	}

    /* close the socket */
    close(sockfd);

	*json      = js;
	*json_toks = tok;
	*json_size = p.toknext;
	return TRUE;
}

static gboolean
json_streq(gchar* js, jsmntok_t *t, gchar* key) {
	return (t->type == JSMN_STRING &&
	        t->end - t->start == strlen(key) &&
			strncmp(key, js+t->start, t->end - t->start) == 0);
}

static gchar*
json_strcpy(gchar* js, jsmntok_t *t) {
	g_assert(t->type == JSMN_STRING);

	size_t len = t->end - t->start;
	gchar* str = malloc(len+1);
	memcpy(str, js+t->start, len);
	str[len] = 0;
	
	return str;
}

static int
json_skip(const char *js, jsmntok_t *t, size_t count) {
	int i, j, k;
	if (count == 0) {
		return 0;
	}
	if (t->type == JSMN_PRIMITIVE || t->type == JSMN_STRING) {
		return 1;
	} else if (t->type == JSMN_OBJECT) {
		j = 0;
		for (i = 0; i < t->size; i++) {
			j += json_skip(js, t+1+j, count-j);
			j += json_skip(js, t+1+j, count-j);
		}
		return j+1;
	} else if (t->type == JSMN_ARRAY) {
		j = 0;
		for (i = 0; i < t->size; i++) {
			j += json_skip(js, t+1+j, count-j);
		}
		return j+1;
	}
	return 0;
}

char rfc3986[256];

static void
url_encoder_rfc_tables_init()
{
    int i;
    for (i = 0; i < 256; i++) {
        rfc3986[i] = isalnum(i) || i == '~' || i == '-' || i == '.' || i == '_' ? i : 0;
    }
}

static void
url_encode(unsigned char *s, char *enc) {
    for (; *s; s++) {
        if (rfc3986[*s])
			*(enc++) = rfc3986[*s];
        else {
			sprintf(enc, "%%%02X", *s);
			while (*++enc);
		}
    }
    *enc = 0;
}

enum
{
  COL_SELECTED = 0,
  COL_CHECKED,
  COL_LANGUAGE,
  COL_CONCRETE,
  NUM_COLS
} ;

typedef struct {
	GeanyPlugin* plugin;
	GtkWidget* main_menu_item;
	GtkListStore* store;
	gboolean lookup_finished;
} WordNetPlugin;

#define CNC_LEN 8

typedef struct {
	GtkListStore* store;
	GtkWidget* view;
	gchar *word;
	WordNetPlugin* wn_data;
	GThread* thread;

	gint  n_cols;
	gchar selected_lang[CNC_LEN+1];
	gchar checked_langs[];
} LookupThread;

typedef struct {
	LookupThread* lookup_thread;
	gchar *cols[];
} LookupRow;

static void
gf_wordnet_linearize(LookupRow* row) {
	char *js;
	jsmntok_t *t;
	size_t count;

	if (!json_request(&js, &t, &count,
	                  "cloud.grammaticalframework.org",
	                  "/robust/Parse.pgf?command=c-linearize&tree=%s&to=%s",
	                  row->cols[0], row->lookup_thread->checked_langs))
        return;

	g_assert(t->type == JSMN_ARRAY);

	int j = 1, i, k;
    for (i = 0; i < t->size; i++) {
		jsmntok_t *obj = t+j;  j++;
		g_assert(obj->type == JSMN_OBJECT);

		gint to = 0;
		for (k = 0; k < obj->size; k++) {
			jsmntok_t *key = t+j;  j++;
			jsmntok_t *val = t+j;  j++;

			if (json_streq(js, key, "to")) {
				gchar *concr = json_strcpy(js, val);
				char *s = strstr(row->lookup_thread->checked_langs, concr);
				if (s != NULL) {
					to = (s-row->lookup_thread->checked_langs)/11+1;
				}
				free(concr);
			} else if (json_streq(js, key, "text")) {
				if (to > 0)
					row->cols[to] = json_strcpy(js, val);
			} else {
				j += json_skip(js, val, count-j+1)-1;
			}
		}
    }

    free(js);
	free(t);
}

static gboolean
on_popup_focus_out (GtkWidget *widget,
                    GdkEventFocus *event,
                    gpointer data)
{
    gtk_widget_destroy (widget);
    return TRUE;
}

static gboolean
add_row (gpointer data)
{
	LookupRow *row = data;

	if (!row->lookup_thread->wn_data->lookup_finished) {
		GtkTreeIter iter;
		gtk_list_store_append (row->lookup_thread->store, &iter);
		
		for (gint i = 0; i < row->lookup_thread->n_cols; i++) {
			if (row->cols[i] != NULL) {
				GValue val = G_VALUE_INIT;
				g_value_init(&val, G_TYPE_STRING);
				g_value_set_string(&val,row->cols[i]);
				gtk_list_store_set_value(GTK_LIST_STORE(row->lookup_thread->store),&iter,i,&val);
			}
		}
	}

	for (gint i = 0; i < row->lookup_thread->n_cols; i++) {
		free(row->cols[i]);
	}
	free(row);

	return G_SOURCE_REMOVE;
}

static gboolean
set_focus (gpointer data)
{
	LookupThread *lookup_thread = data;
	gtk_widget_grab_focus(lookup_thread->view);
	g_signal_connect (G_OBJECT (gtk_widget_get_toplevel(lookup_thread->view)),
					  "focus-out-event",
					  G_CALLBACK (on_popup_focus_out),
					  NULL);

	return G_SOURCE_REMOVE;
}

static void
gf_wordnet_lookup(LookupThread *lookup_thread)
{
	char *js;
	jsmntok_t *t;
	size_t count;

	gchar encoded_word[strlen(lookup_thread->word)*3+1];
	url_encode(lookup_thread->word, encoded_word);

	//////////////////////////////////////////////////////////////
	// 1. Lexical lookup. The result is a list of unique lemmas
	// separated with %20 in variable lemmas
	if (!json_request(&js, &t, &count,
	                  "cloud.grammaticalframework.org",
	                  "/robust/Parse.pgf?command=c-lookupmorpho&input=%s&from=%s",
	                  encoded_word,
	                  lookup_thread->selected_lang))
        return;

	gint lemmas_len = 0;
	gchar* lemmas = NULL;

    g_assert(t->type == JSMN_ARRAY);

	int j = 0, i, k, l, r;
    for (i = 0; i < t->size; i++) {
		jsmntok_t *obj = t+1+j;  j++;
		g_assert(obj->type == JSMN_OBJECT);

		for (k = 0; k < obj->size; k++) {
			jsmntok_t *key = t+1+j;  j++;
			jsmntok_t *val = t+1+j;  j++;

			if (json_streq(js, key, "lemma")) {
				g_assert(val->type == JSMN_STRING);

				size_t len = val->end - val->start;

				gboolean found = FALSE;
				if (lemmas != NULL) {
					for (l = 0; l <= ((int) lemmas_len)-((int) len); l++) {
						if (strncmp(lemmas+l, js+val->start, len) == 0) {
							found = TRUE;
							break;
						}
					}
				}

				if (!found) {
					lemmas = realloc_it(lemmas, lemmas_len + len + 4);
					if (lemmas == NULL) {
						free(js);
						free(t);
						return;
					}
					if (lemmas_len > 0) {
						memcpy(lemmas+lemmas_len, "%20", 3);
						lemmas_len += 3;
					}
					memcpy(lemmas+lemmas_len, js+val->start, len);
					lemmas_len += len;
					lemmas[lemmas_len] = 0;
				}
			}
		}
    }

    free(js);
	free(t);

	//////////////////////////////////////////////////////////////
	// 2. Semantic lookup.
    if (!json_request(&js, &t, &count,
	                  "cloud.grammaticalframework.org",
	                  "/wordnet/SenseService.fcgi?lexical_ids=%s",
	                  lemmas)) {
		free(lemmas);
        return;
	}
	free(lemmas);

	j = 1;
    g_assert(t->type == JSMN_OBJECT);
	jsmntok_t *result = NULL;
	for (k = 0; k < t->size; k++) {
		jsmntok_t *key = t+j;  j++;
		jsmntok_t *val = t+j;  j++;
		if (json_streq(js, key, "result")) {
			result = val;
			break;
		}
	}
	g_assert(result != NULL);

    for (i = 0; i < result->size; i++) {
		jsmntok_t *obj = t+j;  j++;
		g_assert(obj->type == JSMN_OBJECT);

		gchar* gloss = NULL;
		for (k = 0; k < obj->size; k++) {
			jsmntok_t *key = t+j;  j++;
			jsmntok_t *val = t+j;  j++;

			if (json_streq(js, key, "gloss") && gloss == NULL) {
				gloss = json_strcpy(js, val);
			} else if (json_streq(js, key, "lex_ids")) {
				g_assert(val->type == JSMN_OBJECT);

				jsmntok_t *lex_obj = val;
				for (l = 0; l < lex_obj->size; l++) {
					jsmntok_t *lex_id   = t+j;  j++;
					jsmntok_t *lex_info = t+j;  j++;

					gboolean match = FALSE;
					for (r = 0; r < lex_info->size; r++) {
						jsmntok_t *key = t+j;  j++;
						jsmntok_t *val = t+j;  j++;

						if (json_streq(js, key, "match")) {
							match = TRUE;
						}

						j += json_skip(js, val, count-j+1)-1;
					}

					if (match) {
						LookupRow* row = malloc(sizeof(LookupRow)+
						                        sizeof(gchar*)*lookup_thread->n_cols);
						row->lookup_thread = lookup_thread;
						memset(row->cols,sizeof(gchar*)*lookup_thread->n_cols,0);

						row->cols[0] = json_strcpy(js, lex_id);
						gf_wordnet_linearize(row);
						row->cols[lookup_thread->n_cols-1] = strdup(gloss);

						gdk_threads_add_idle(add_row, row);
					}
				}
			} else {
				j += json_skip(js, val, count-j+1)-1;
			}
		}
		free(gloss);
    }

    free(js);
	free(t);

    gdk_threads_add_idle(set_focus, lookup_thread);
}

typedef struct {
	ScintillaObject *sci;
	gint start, end;
} SelectionData;

static void
row_activated_cb (GtkTreeView       *tree_view,
                  GtkTreePath       *path,
                  GtkTreeViewColumn *column,
                  gpointer           user_data)
{
	SelectionData* sdata = (SelectionData*) user_data;

	GtkTreeModel *model = gtk_tree_view_get_model(tree_view);

	GtkTreeIter iter;
	gtk_tree_model_get_iter (model, &iter, path);

	gchar* abs_id;
	gtk_tree_model_get (model, &iter,
                        0, &abs_id,
                        -1);
	sci_set_selection_start(sdata->sci, sdata->start);
	sci_set_selection_end(sdata->sci, sdata->end);
	sci_replace_sel(sdata->sci, abs_id);
	g_free(abs_id);

	gtk_widget_destroy(gtk_widget_get_toplevel(GTK_WIDGET(tree_view)));
}

static gboolean
on_popup_key_press (GtkWidget *widget,
                    GdkEventKey *event,
                    gpointer data)
{
    switch (event->keyval)
    {
      case GDK_KEY_Escape:
	    gtk_widget_destroy (widget);
        break;
    }

    return FALSE; 
}

static void
on_popup_destroy(GtkWidget *widget,
                 gpointer  data)
{
	LookupThread *lookup_thread = data;

	lookup_thread->wn_data->lookup_finished = TRUE;

	g_object_unref(lookup_thread->store);
	g_free(lookup_thread->word);
	g_thread_unref(lookup_thread->thread);
	g_free(lookup_thread);
}

static gchar*
get_current_word_range(ScintillaObject *sci, SelectionData* sdata)
{
	char* delimiters = " \t\r\n()[]{};:.?!,<>|*+/\\\"#¤=^~'´`0123456789_";

	gint start, end;
	if (sci_has_selection(sci)) {
		start = sci_get_selection_start(sci);
		end   = sci_get_selection_end(sci);
	} else {
		start = (end = sci_get_current_position(sci));

		gint len = sci_get_length(sci);

		while (start > 1 &&
		       strchr(delimiters, sci_get_char_at(sci, start-1)) == NULL) {
			start--;
		}
		while (end < len &&
		       strchr(delimiters, sci_get_char_at(sci, end)) == NULL) {
			end++;
		}
	}

	if (start >= end)
		return NULL;

	sdata->sci   = sci;
	sdata->start = start;
	sdata->end   = end;

	return sci_get_contents_range(sci,start,end);
}

static void
destroy_sdata(gpointer data, GClosure *closure) {
	free(data);
}

static gpointer
lookup_thread_cb(gpointer data) {
	LookupThread *lookup_thread = data;
	gf_wordnet_lookup(lookup_thread);
	return NULL;
}

static void
item_activate_cb(GtkMenuItem *menuitem, gpointer user_data)
{
	WordNetPlugin* wn_data = (WordNetPlugin*) user_data;
    GeanyDocument* doc = document_get_current();

	if (!wn_data->lookup_finished)
		return;

	size_t n_langs =
		gtk_tree_model_iter_n_children(GTK_TREE_MODEL(wn_data->store), NULL);

    LookupThread* lookup_thread = malloc(sizeof(LookupThread)+(CNC_LEN+3)*n_langs+1);
    lookup_thread->wn_data = wn_data;
    lookup_thread->n_cols = 0;

    SelectionData* sdata = malloc(sizeof(SelectionData));
    lookup_thread->word = get_current_word_range(doc->editor->sci, sdata);
    if (lookup_thread->word == NULL) {
		free(sdata);
		free(lookup_thread);
		return;
	}

	gint x = scintilla_send_message (doc->editor->sci, SCI_POINTXFROMPOSITION, 0, sdata->start);
	gint y = scintilla_send_message (doc->editor->sci, SCI_POINTYFROMPOSITION, 0, sdata->start);

	gint win_x, win_y;
	gdk_window_get_origin (
		gtk_widget_get_window(GTK_WIDGET(doc->editor->sci)),
		&win_x, &win_y);

    GtkWidget *popup_window;
    popup_window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_widget_set_can_focus(popup_window, TRUE);
    gtk_window_set_title (GTK_WINDOW (popup_window), "GF WordNet");
    gtk_container_set_border_width (GTK_CONTAINER (popup_window), 3);
    gtk_window_set_resizable(GTK_WINDOW (popup_window), FALSE);
    gtk_window_set_decorated(GTK_WINDOW (popup_window), FALSE);
    gtk_widget_set_size_request (popup_window, 550, 150);
    gtk_window_set_skip_taskbar_hint (GTK_WINDOW (popup_window), TRUE);
    gtk_window_set_skip_pager_hint (GTK_WINDOW (popup_window), TRUE);
    gtk_window_set_transient_for(GTK_WINDOW (popup_window),GTK_WINDOW (wn_data->plugin->geany_data->main_widgets->window));
    gtk_window_move (GTK_WINDOW (popup_window), win_x+x, win_y+y+20);
    gtk_widget_set_events(popup_window, GDK_FOCUS_CHANGE_MASK);
    g_signal_connect (G_OBJECT (popup_window),
                      "key-press-event",
                      G_CALLBACK(on_popup_key_press),
                      NULL);
    g_signal_connect (G_OBJECT (popup_window),
                      "destroy",
                      G_CALLBACK(on_popup_destroy),
                      lookup_thread);

    GtkWidget* view = gtk_tree_view_new ();

    GType* types = 
		malloc((n_langs+2)*sizeof(GType));

	types[lookup_thread->n_cols] = G_TYPE_STRING;
    GtkCellRenderer *renderer = gtk_cell_renderer_text_new ();
    gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (view),
                                                 -1,      
                                                 "Abstract",
                                                 renderer,
                                                 "text", lookup_thread->n_cols++,
                                                 NULL);

	gint  pos = 0;
    GtkTreeIter iter;    
    gboolean more =
        gtk_tree_model_get_iter_first(GTK_TREE_MODEL(wn_data->store),
                                      &iter);
    while (more) {
		GValue sel_value = G_VALUE_INIT;
		gtk_tree_model_get_value(GTK_TREE_MODEL(wn_data->store), &iter, COL_SELECTED, &sel_value);

		GValue check_value = G_VALUE_INIT;
		gtk_tree_model_get_value(GTK_TREE_MODEL(wn_data->store), &iter, COL_CHECKED, &check_value);

		GValue lang_value = G_VALUE_INIT;
		gtk_tree_model_get_value(GTK_TREE_MODEL(wn_data->store), &iter, COL_LANGUAGE, &lang_value);

		GValue cnc_value = G_VALUE_INIT;
		gtk_tree_model_get_value(GTK_TREE_MODEL(wn_data->store), &iter, COL_CONCRETE, &cnc_value);

		const gchar* cnc = g_value_get_string(&cnc_value);

		if (g_value_get_boolean(&sel_value)) {
			strcpy(lookup_thread->selected_lang,cnc);
		}

		if (g_value_get_boolean(&check_value)) {
			types[lookup_thread->n_cols] = G_TYPE_STRING;
			renderer = gtk_cell_renderer_text_new ();
			gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW(view),
														 -1,
														 g_value_get_string(&lang_value),
														 renderer,
														 "text", lookup_thread->n_cols++,
														 NULL);

			if (pos > 0) {
				strcpy(lookup_thread->checked_langs+pos,"%20");
				pos += 3;
			}
			strcpy(lookup_thread->checked_langs+pos,cnc);
			pos += strlen(cnc);
		}

		g_value_unset(&cnc_value);
		g_value_unset(&lang_value);
		g_value_unset(&check_value);
		g_value_unset(&sel_value);

		more = 
		    gtk_tree_model_iter_next(GTK_TREE_MODEL(wn_data->store),
                                     &iter);
    }

	types[lookup_thread->n_cols] = G_TYPE_STRING;
    renderer = gtk_cell_renderer_text_new ();
    gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (view),
                                                 -1,
                                                 "Definition",
                                                 renderer,
                                                 "text", lookup_thread->n_cols++,
                                                 NULL);

    g_signal_connect_data (view, "row-activated",
        G_CALLBACK(row_activated_cb), sdata,
        destroy_sdata, 0);

    lookup_thread->store = gtk_list_store_newv (lookup_thread->n_cols, types);
    lookup_thread->view  = view;

    free(types);

    gtk_tree_view_set_model (GTK_TREE_VIEW (view),
                             GTK_TREE_MODEL(lookup_thread->store));

    GtkWidget *scrolled_window = gtk_scrolled_window_new (NULL, NULL);
    gtk_container_add (GTK_CONTAINER (scrolled_window), view);
    gtk_container_add (GTK_CONTAINER(popup_window), scrolled_window);

    gtk_widget_show_all (popup_window);

	wn_data->lookup_finished = FALSE;
    lookup_thread->thread =
        g_thread_new ("lookup_thread",
                      lookup_thread_cb,
                      lookup_thread);
}

static gboolean
kb_activate_cb(GeanyKeyBinding *key, guint key_id, gpointer user_data)
{
	item_activate_cb(NULL, user_data);
	return TRUE;
}

static gboolean
gf_wordnet_init(GeanyPlugin *plugin, gpointer pdata)
{
	WordNetPlugin* wn_data = malloc(sizeof(WordNetPlugin));
	wn_data->plugin = plugin;
    wn_data->lookup_finished = TRUE;
    geany_plugin_set_data(plugin, wn_data, free);

    // Create a new menu item and show it
    wn_data->main_menu_item = gtk_menu_item_new_with_mnemonic("GF WordNet Search");
    gtk_widget_show(wn_data->main_menu_item);
    ui_add_document_sensitive(wn_data->main_menu_item);
    // Attach the new menu item to the Tools menu
    gtk_container_add(GTK_CONTAINER(plugin->geany_data->main_widgets->tools_menu),
        wn_data->main_menu_item);
    // Connect the menu item with a callback function
    // which is called when the item is clicked
    g_signal_connect(wn_data->main_menu_item, "activate",
        G_CALLBACK(item_activate_cb), wn_data);

    GeanyKeyGroup* key_group =
		plugin_set_key_group(plugin, "gf_wordnet_chars", KB_GF_WORDNET, NULL);
	keybindings_set_item_full(key_group, KB_LOOKUP_WORD,
	    GDK_w, GDK_MOD1_MASK, "Alt+W",
	    "lookup word", wn_data->main_menu_item,
	    kb_activate_cb,
		wn_data, NULL);

	/* create list store */
	wn_data->store =
		gtk_list_store_new (NUM_COLS, G_TYPE_BOOLEAN, G_TYPE_BOOLEAN, G_TYPE_STRING, G_TYPE_STRING);

	GtkTreeIter iter;
	gtk_list_store_append(GTK_LIST_STORE(wn_data->store), &iter);
	gtk_list_store_set(GTK_LIST_STORE(wn_data->store), &iter,
		COL_SELECTED, FALSE,
		COL_CHECKED,  TRUE,
		COL_LANGUAGE, "Bulgarian",
		COL_CONCRETE, "ParseBul", -1);

	gtk_list_store_append(GTK_LIST_STORE(wn_data->store), &iter);
	gtk_list_store_set(GTK_LIST_STORE(wn_data->store), &iter,
		COL_SELECTED, FALSE,
		COL_CHECKED, FALSE,
		COL_LANGUAGE, "Catalan",
		COL_CONCRETE, "ParseCat", -1);

	gtk_list_store_append(GTK_LIST_STORE(wn_data->store), &iter);
	gtk_list_store_set(GTK_LIST_STORE(wn_data->store), &iter,
		COL_SELECTED, FALSE,
		COL_CHECKED, FALSE,
		COL_LANGUAGE, "Chinese",
		COL_CONCRETE, "ParseChi", -1);

	gtk_list_store_append(GTK_LIST_STORE(wn_data->store), &iter);
	gtk_list_store_set(GTK_LIST_STORE(wn_data->store), &iter,
		COL_SELECTED, FALSE,
		COL_CHECKED, FALSE,
		COL_LANGUAGE, "Dutch",
		COL_CONCRETE, "ParseDut", -1);

	gtk_list_store_append(GTK_LIST_STORE(wn_data->store), &iter);
	gtk_list_store_set(GTK_LIST_STORE(wn_data->store), &iter,
		COL_SELECTED, TRUE,
		COL_CHECKED, TRUE,
		COL_LANGUAGE, "English",
		COL_CONCRETE, "ParseEng", -1);

	gtk_list_store_append(GTK_LIST_STORE(wn_data->store), &iter);
	gtk_list_store_set(GTK_LIST_STORE(wn_data->store), &iter,
		COL_SELECTED, FALSE,
		COL_CHECKED, FALSE,
		COL_LANGUAGE, "Estonian",
		COL_CONCRETE, "ParseEst", -1);

	gtk_list_store_append(GTK_LIST_STORE(wn_data->store), &iter);
	gtk_list_store_set(GTK_LIST_STORE(wn_data->store), &iter,
		COL_SELECTED, FALSE,
		COL_CONCRETE, FALSE,
		COL_LANGUAGE, "Finnish",
		COL_CONCRETE, "ParseFin", -1);

	gtk_list_store_append(GTK_LIST_STORE(wn_data->store), &iter);
	gtk_list_store_set(GTK_LIST_STORE(wn_data->store), &iter,
		COL_SELECTED, FALSE,
		COL_CONCRETE, FALSE,
		COL_LANGUAGE, "Italian",
		COL_CONCRETE, "ParseIta", -1);

	gtk_list_store_append(GTK_LIST_STORE(wn_data->store), &iter);
	gtk_list_store_set(GTK_LIST_STORE(wn_data->store), &iter,
		COL_SELECTED, FALSE,
		COL_CONCRETE, FALSE,
		COL_LANGUAGE, "Portuguese",
		COL_CONCRETE, "ParsePor", -1);

	gtk_list_store_append(GTK_LIST_STORE(wn_data->store), &iter);
	gtk_list_store_set(GTK_LIST_STORE(wn_data->store), &iter,
		COL_SELECTED, FALSE,
		COL_CONCRETE, FALSE,
		COL_LANGUAGE, "Slovenian",
		COL_CONCRETE, "ParseSlv", -1);

	gtk_list_store_append(GTK_LIST_STORE(wn_data->store), &iter);
	gtk_list_store_set(GTK_LIST_STORE(wn_data->store), &iter,
		COL_SELECTED, FALSE,
		COL_CONCRETE, FALSE,
		COL_LANGUAGE, "Spanish",
		COL_CONCRETE, "ParseSpa", -1);

	gtk_list_store_append(GTK_LIST_STORE(wn_data->store), &iter);
	gtk_list_store_set(GTK_LIST_STORE(wn_data->store), &iter,
		COL_SELECTED, FALSE,
		COL_CHECKED, TRUE,
		COL_LANGUAGE, "Swedish",
		COL_CONCRETE, "ParseSwe", -1);

	gtk_list_store_append(GTK_LIST_STORE(wn_data->store), &iter);
	gtk_list_store_set(GTK_LIST_STORE(wn_data->store), &iter,
		COL_SELECTED, FALSE,
		COL_CHECKED, FALSE,
		COL_LANGUAGE, "Thai",
		COL_CONCRETE, "ParseTha", -1);

	gtk_list_store_append(GTK_LIST_STORE(wn_data->store), &iter);
	gtk_list_store_set(GTK_LIST_STORE(wn_data->store), &iter,
		COL_SELECTED, FALSE,
		COL_CHECKED, FALSE,
		COL_LANGUAGE, "Turkish",
		COL_CONCRETE, "ParseTur", -1);

    url_encoder_rfc_tables_init();

    return TRUE;
}

static void language_selected_cb(GtkCellRendererToggle *cell,
                                 gchar                 *path_string,
                                 gpointer               user_data)
{
	WordNetPlugin* wn_data = user_data;

	GValue val = G_VALUE_INIT;
	g_value_init(&val, G_TYPE_BOOLEAN);
	g_value_set_boolean(&val,FALSE);

    GtkTreeIter iter;    
    gboolean more =
        gtk_tree_model_get_iter_first(GTK_TREE_MODEL(wn_data->store),
                                      &iter);
    while (more) {
		gtk_list_store_set_value(GTK_LIST_STORE(wn_data->store),&iter,COL_SELECTED,&val);

		more = 
		    gtk_tree_model_iter_next(GTK_TREE_MODEL(wn_data->store),
                                     &iter);
    }

	gtk_tree_model_get_iter_from_string(GTK_TREE_MODEL(wn_data->store),&iter,path_string);
	
	g_value_set_boolean(&val,TRUE);
	gtk_list_store_set_value(GTK_LIST_STORE(wn_data->store),&iter,COL_SELECTED,&val);
}

static void language_checked_cb(GtkCellRendererToggle *cell,
                                gchar                 *path_string,
                                gpointer               user_data)
{
	WordNetPlugin* wn_data = user_data;

    GtkTreeIter iter;
	gtk_tree_model_get_iter_from_string(GTK_TREE_MODEL(wn_data->store),&iter,path_string);

	GValue val = G_VALUE_INIT;
	g_value_init(&val, G_TYPE_BOOLEAN);
	gtk_tree_model_get_value(GTK_TREE_MODEL(wn_data->store), &iter, COL_CHECKED, &val);
	g_value_set_boolean(&val,!g_value_get_boolean(&val));
	gtk_list_store_set_value(GTK_LIST_STORE(wn_data->store), &iter, COL_CHECKED, &val);
}

static GtkWidget* gf_wordnet_configure(GeanyPlugin *plugin, GtkDialog *parent, gpointer pdata)
{
	GtkWidget *lbox;
	GtkTreeViewColumn *column;
	GtkCellRenderer *render;

	WordNetPlugin* wn_data = pdata;

	/* create tree view */
	lbox = gtk_tree_view_new_with_model(GTK_TREE_MODEL(wn_data->store));
	gtk_tree_selection_set_mode(gtk_tree_view_get_selection(GTK_TREE_VIEW(lbox)), GTK_SELECTION_SINGLE);

	/* add columns to the tree view */
	render = gtk_cell_renderer_toggle_new();
	gtk_cell_renderer_toggle_set_activatable(GTK_CELL_RENDERER_TOGGLE(render), TRUE);
	gtk_cell_renderer_toggle_set_radio(GTK_CELL_RENDERER_TOGGLE(render), TRUE);
	g_signal_connect (render, "toggled",
        G_CALLBACK(language_selected_cb), wn_data);
	column = gtk_tree_view_column_new_with_attributes ("",
				render,
				"active",
				COL_SELECTED,
				NULL);
	gtk_tree_view_append_column (GTK_TREE_VIEW(lbox), column);

	render = gtk_cell_renderer_toggle_new();
	gtk_cell_renderer_toggle_set_activatable(GTK_CELL_RENDERER_TOGGLE(render), TRUE);
	g_signal_connect (render, "toggled",
        G_CALLBACK(language_checked_cb), wn_data);
	column = gtk_tree_view_column_new_with_attributes ("",
				render,
				"active",
				COL_CHECKED,
				NULL);
	gtk_tree_view_append_column (GTK_TREE_VIEW(lbox), column);

	column = gtk_tree_view_column_new_with_attributes ("Language",
				gtk_cell_renderer_text_new (),
				"text",
				COL_LANGUAGE,
				NULL);
	gtk_tree_view_append_column (GTK_TREE_VIEW(lbox), column);

	return lbox;
}

static void gf_wordnet_cleanup(GeanyPlugin *plugin, gpointer pdata)
{
    WordNetPlugin *wn_data = (WordNetPlugin *) pdata;

    gtk_widget_destroy(wn_data->main_menu_item);
    g_object_unref (wn_data->store);
}

void geany_load_module(GeanyPlugin *plugin) {
	/* Step 1: Set metadata */
    plugin->info->name = "GF WordNet Plugin";
    plugin->info->description = "A tool for searching through GF WordNet while developing grammars";
    plugin->info->version = "0.1";
    plugin->info->author = "Krasimir Angelov <krasimir@digitalgrammars.com>";
    /* Step 2: Set functions */
    plugin->funcs->init = gf_wordnet_init;
    plugin->funcs->configure = gf_wordnet_configure;
    plugin->funcs->cleanup = gf_wordnet_cleanup;

    /* Step 3: Register! */
    GEANY_PLUGIN_REGISTER(plugin, 225);
}
