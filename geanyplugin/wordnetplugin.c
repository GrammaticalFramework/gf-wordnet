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


enum
{
  COL_ABSTRACT = 0,
  COL_ENGLISH,
  COL_SWEDISH,
  COL_BULGARIAN,
  COL_DEFINITION,
  NUM_COLS
} ;

static void
gf_wordnet_linearize(gchar* abs, gchar** eng, gchar** swe, gchar** bul) {
	char *js;
	jsmntok_t *t;
	size_t count;

	*eng = NULL;
	*swe = NULL;
	*bul = NULL;

	if (!json_request(&js, &t, &count,
	                  "cloud.grammaticalframework.org",
	                  "/robust/Parse.pgf?command=c-linearize&to=ParseEng%20ParseSwe%20ParseBul&tree=%s",
	                  abs))
        return;

	g_assert(t->type == JSMN_ARRAY);

	int j = 1, i, k;
    for (i = 0; i < t->size; i++) {
		jsmntok_t *obj = t+j;  j++;
		g_assert(obj->type == JSMN_OBJECT);

		gchar** to = NULL;
		for (k = 0; k < obj->size; k++) {
			jsmntok_t *key = t+j;  j++;
			jsmntok_t *val = t+j;  j++;

			if (json_streq(js, key, "to")) {
				if (json_streq(js, val, "ParseEng"))
					to = eng;
				else if (json_streq(js, val, "ParseSwe"))
					to = swe;
				else if (json_streq(js, val, "ParseBul"))
					to = bul;
			} else if (json_streq(js, key, "text")) {
				if (to != NULL)
					*to = json_strcpy(js, val);
			} else {
				j += json_skip(js, val, count-j+1)-1;
			}
		}
    }

    free(js);
	free(t);
}

static gboolean lookup_finished = TRUE;

typedef struct {
	GtkListStore* store;
	gchar *abs, *eng, *swe, *bul, *gloss;
} LookupRow;

static gboolean
add_row (gpointer data)
{
	LookupRow *row = data;

	if (!lookup_finished) {
		GtkTreeIter iter;
		gtk_list_store_append (row->store, &iter);
		gtk_list_store_set (row->store, &iter,
							COL_ABSTRACT, row->abs,
							COL_ENGLISH,  row->eng,
							COL_SWEDISH,  row->swe,
							COL_BULGARIAN,  row->bul,
							COL_DEFINITION, row->gloss,
							-1);
	}

	free(row->abs);
	free(row->eng);
	free(row->swe);
	free(row->bul);
	free(row->gloss);
	free(row);

	return G_SOURCE_REMOVE;
}

static void
gf_wordnet_lookup(GtkListStore* store, gchar *word)
{
	char *js;
	jsmntok_t *t;
	size_t count;

	//////////////////////////////////////////////////////////////
	// 1. Lexical lookup. The result is a list of unique lemmas
	// separated with %20 in variable lemmas
	if (!json_request(&js, &t, &count,
	                  "cloud.grammaticalframework.org",
	                  "/robust/Parse.pgf?command=c-lookupmorpho&input=%s&from=ParseEng",
	                  word))
        return;

	gint lemmas_len = 0;
	gchar* lemmas = NULL;

    g_assert(t->type == JSMN_ARRAY);

	int j = 0, i, k, l;
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
	                  "www.grammaticalframework.org",
	                  "/~krasimir/SenseService.fcgi?lexical_ids=%s",
	                  lemmas)) {
		free(lemmas);
        return;
	}
	free(lemmas);

    g_assert(t->type == JSMN_ARRAY);

	j = 1;
    for (i = 0; i < t->size; i++) {
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
					jsmntok_t *key = t+j;  j++;
					jsmntok_t *val = t+j;  j++;

					LookupRow* row = malloc(sizeof(LookupRow));
					row->store = store;

					row->abs = json_strcpy(js, key);
					gf_wordnet_linearize(row->abs,
					                    &row->eng,
					                    &row->swe,
					                    &row->bul);

					row->eng = row->eng ? row->eng : strdup("");
					row->swe = row->swe ? row->swe : strdup("");
					row->bul = row->bul ? row->bul : strdup("");
					row->gloss = strdup(gloss);

					gdk_threads_add_idle(add_row, row);

					j += json_skip(js, val, count-j+1)-1;
				}
			} else {
				j += json_skip(js, val, count-j+1)-1;
			}
		}
		free(gloss);
    }

    free(js);
	free(t);
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
                        COL_ABSTRACT, &abs_id,
                        -1);
	sci_set_selection_start(sdata->sci, sdata->start);
	sci_set_selection_end(sdata->sci, sdata->end);
	sci_replace_sel(sdata->sci, abs_id);
	g_free(abs_id);

	gtk_widget_destroy(gtk_widget_get_toplevel(GTK_WIDGET(tree_view)));
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

typedef struct {
	GtkListStore* store;
	gchar *word;
	GThread* thread;
} LookupThread;

static void
on_popup_destroy(GtkWidget *widget,
                 gpointer  data)
{
	LookupThread *lookup_thread = data;

	lookup_finished = TRUE;

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
	gf_wordnet_lookup(lookup_thread->store, lookup_thread->word);
}

static void
item_activate_cb(GtkMenuItem *menuitem, gpointer user_data)
{
	if (!lookup_finished)
		return;

	GeanyPlugin* plugin = (GeanyPlugin*) user_data;
    GeanyDocument *doc = document_get_current();

    LookupThread* lookup_thread = malloc(sizeof(LookupThread));

    SelectionData* sdata = malloc(sizeof(SelectionData));
    lookup_thread->word = get_current_word_range(doc->editor->sci, sdata);

	gint x = scintilla_send_message (doc->editor->sci, SCI_POINTXFROMPOSITION, 0, sdata->start);
	gint y = scintilla_send_message (doc->editor->sci, SCI_POINTYFROMPOSITION, 0, sdata->start);

	gint win_x, win_y;
	gdk_window_get_origin (
		gtk_widget_get_window(GTK_WIDGET(doc->editor->sci)),
		&win_x, &win_y);

    GtkWidget *popup_window;
    popup_window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_widget_set_can_focus(popup_window, TRUE);
    gtk_window_set_title (GTK_WINDOW (popup_window), "Pop Up window");
    gtk_container_set_border_width (GTK_CONTAINER (popup_window), 3);
    gtk_window_set_resizable(GTK_WINDOW (popup_window), FALSE);
    gtk_window_set_decorated(GTK_WINDOW (popup_window), FALSE);
    gtk_widget_set_size_request (popup_window, 550, 150);
    gtk_window_set_type_hint(GTK_WINDOW(popup_window), GDK_WINDOW_TYPE_HINT_POPUP_MENU);
    gtk_window_set_transient_for(GTK_WINDOW (popup_window),GTK_WINDOW (plugin->geany_data->main_widgets->window));
    gtk_window_move (GTK_WINDOW (popup_window), win_x+x, win_y+y+20);
    gtk_widget_set_events(popup_window, GDK_FOCUS_CHANGE_MASK);
    g_signal_connect (G_OBJECT (popup_window),
                      "focus-out-event",
                      G_CALLBACK (on_popup_focus_out),
                      NULL);
    g_signal_connect (G_OBJECT (popup_window),
                      "key-press-event",
                      G_CALLBACK(on_popup_key_press),
                      NULL);
    g_signal_connect (G_OBJECT (popup_window),
                      "destroy",
                      G_CALLBACK(on_popup_destroy),
                      lookup_thread);

    GtkWidget* view = gtk_tree_view_new ();

    GtkCellRenderer *renderer = gtk_cell_renderer_text_new ();
    gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (view),
                                                 -1,      
                                                 "Abstract",
                                                 renderer,
                                                 "text", COL_ABSTRACT,
                                                 NULL);

    renderer = gtk_cell_renderer_text_new ();
    gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (view),
                                                 -1,      
                                                 "English",  
                                                 renderer,
                                                 "text", COL_ENGLISH,
                                                 NULL);

    renderer = gtk_cell_renderer_text_new ();
    gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (view),
                                                 -1,      
                                                 "Swedish",  
                                                 renderer,
                                                 "text", COL_SWEDISH,
                                                 NULL);

    renderer = gtk_cell_renderer_text_new ();
    gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (view),
                                                 -1,
                                                 "Bulgarian",
                                                 renderer,
                                                 "text", COL_BULGARIAN,
                                                 NULL);

    renderer = gtk_cell_renderer_text_new ();
    gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (view),
                                                 -1,
                                                 "Definition",
                                                 renderer,
                                                 "text", COL_DEFINITION,
                                                 NULL);

    g_signal_connect_data (view, "row-activated",
        G_CALLBACK(row_activated_cb), sdata,
        destroy_sdata, 0);

    lookup_thread->store =
        gtk_list_store_new (NUM_COLS, G_TYPE_STRING, G_TYPE_STRING,
                                      G_TYPE_STRING, G_TYPE_STRING,
                                      G_TYPE_STRING);

    gtk_tree_view_set_model (GTK_TREE_VIEW (view),
                             GTK_TREE_MODEL(lookup_thread->store));

    GtkWidget *scrolled_window = gtk_scrolled_window_new (NULL, NULL);
    gtk_container_add (GTK_CONTAINER (scrolled_window), view);
    gtk_container_add (GTK_CONTAINER(popup_window), scrolled_window);

    gtk_widget_show_all (popup_window);

	lookup_finished = FALSE;
    lookup_thread->thread =
        g_thread_new ("lookup_thread",
                      lookup_thread_cb,
                      lookup_thread);

	gtk_widget_grab_focus (view);
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
    GtkWidget *main_menu_item;
    // Create a new menu item and show it
    main_menu_item = gtk_menu_item_new_with_mnemonic("GF WordNet Search");
    gtk_widget_show(main_menu_item);
    ui_add_document_sensitive(main_menu_item);
    // Attach the new menu item to the Tools menu
    gtk_container_add(GTK_CONTAINER(plugin->geany_data->main_widgets->tools_menu),
        main_menu_item);
    // Connect the menu item with a callback function
    // which is called when the item is clicked
    g_signal_connect(main_menu_item, "activate",
        G_CALLBACK(item_activate_cb), plugin);

    GeanyKeyGroup* key_group =
		plugin_set_key_group(plugin, "gf_wordnet_chars", KB_GF_WORDNET, NULL);
	keybindings_set_item_full(key_group, KB_LOOKUP_WORD,
	    GDK_w, GDK_MOD1_MASK, "Alt+W",
	    "lookup word", main_menu_item,
	    kb_activate_cb,
		plugin, NULL);

    geany_plugin_set_data(plugin, main_menu_item, NULL);

    return TRUE;
}

static void gf_wordnet_cleanup(GeanyPlugin *plugin, gpointer pdata)
{
    GtkWidget *main_menu_item = (GtkWidget *) pdata;

    gtk_widget_destroy(main_menu_item);
}

void geany_load_module(GeanyPlugin *plugin) {
	/* Step 1: Set metadata */
    plugin->info->name = "GF WordNet Plugin";
    plugin->info->description = "A tool for searching through GF WordNet while developing grammars";
    plugin->info->version = "0.1";
    plugin->info->author = "Krasimir Angelov <krasimir@digitalgrammars.com>";
    /* Step 2: Set functions */
    plugin->funcs->init = gf_wordnet_init;
    plugin->funcs->cleanup = gf_wordnet_cleanup;
    /* Step 3: Register! */
    GEANY_PLUGIN_REGISTER(plugin, 225);
}
