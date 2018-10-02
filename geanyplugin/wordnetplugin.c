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

gboolean gf_wordnet_lookup(gchar *word, char** json, jsmntok_t **json_toks, size_t* json_size)
{
    /* first what are we going to send and where are we going to send it? */
    int portno =        80;
    char *host =        "cloud.grammaticalframework.org";
    char *message_fmt = "GET /robust/Parse.pgf?command=c-lookupmorpho&input=%s&from=ParseEng HTTP/1.1\r\nhost: %s\r\n\r\n";

    struct hostent *server;
    struct sockaddr_in serv_addr;
    int sockfd, bytes, sent, total;
    char buffer[1024];

    /* fill in the parameters */
    sprintf(buffer,message_fmt,word,host);

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

enum
{
  COL_ABSTRACT = 0,
  COL_ENGLISH,
  COL_SWEDISH,
  COL_BULGARIAN,
  NUM_COLS
} ;

static void
populate_suggestions_list(GtkListStore* store, const char *js, jsmntok_t *t, size_t count)
{
    GtkWidget* view;
    GtkCellRenderer* renderer;

    g_assert(t->type == JSMN_ARRAY);

	gint lemma_count = 0;
	gchar* lemmas[t->size];

    GtkTreeIter iter;
	int j = 0, i, k, l;
    for (i = 0; i < t->size; i++) {
		jsmntok_t *obj = t+1+j;  j++;
		g_assert(obj->type == JSMN_OBJECT);

		for (k = 0; k < obj->size; k++) {
			jsmntok_t *key = t+1+j;  j++;
			g_assert(key->type == JSMN_STRING);
			
			jsmntok_t *val = t+1+j;  j++;

			char* lemma = "lemma";
			if (key->end - key->start == strlen(lemma) &&
			    strncmp(lemma, js+key->start, key->end - key->start) == 0) {

				g_assert(val->type == JSMN_STRING);

				size_t len = val->end - val->start;
				gchar* fun = malloc(len + 1);
				memcpy(fun, js+val->start, len);
				fun[len] = 0;

				gboolean found = FALSE;
				for (l = 0; l < lemma_count; l++) {
					if (strcmp(lemmas[l], fun) == 0) {
						free(fun);
						found = TRUE;
						break;
					}
				}

				if (!found) {
					lemmas[lemma_count++] = fun;

					gtk_list_store_append (store, &iter);
					gtk_list_store_set (store, &iter,
										COL_ABSTRACT, fun,
										-1);
				}
			}
		}
    }

    for (l = 0; l < lemma_count; l++) {
		free(lemmas[l]);
	}
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

gboolean
on_popup_key_press (GtkWidget *widget,
                    GdkEventKey *event,
                    gpointer user_data)
{
    switch (event->keyval)
    {
      case GDK_KEY_Escape:
	    gtk_widget_destroy (widget);
        break;
    }

    return FALSE; 
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

static void item_activate_cb(GtkMenuItem *menuitem, gpointer user_data)
{
	GeanyPlugin* plugin = (GeanyPlugin*) user_data;
    GeanyDocument *doc = document_get_current();

    SelectionData* sdata = malloc(sizeof(SelectionData));
    gchar* word = get_current_word_range(doc->editor->sci, sdata);

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
    gtk_widget_set_size_request (popup_window, 450, 150);
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

    g_signal_connect_data (view, "row-activated",
        G_CALLBACK(row_activated_cb), sdata,
        destroy_sdata, 0);

    GtkListStore  *store = 
        gtk_list_store_new (NUM_COLS, G_TYPE_STRING, G_TYPE_STRING,
                                      G_TYPE_STRING, G_TYPE_STRING);

    gtk_tree_view_set_model (GTK_TREE_VIEW (view), GTK_TREE_MODEL(store));

    GtkWidget *scrolled_window = gtk_scrolled_window_new (NULL, NULL);
    gtk_container_add (GTK_CONTAINER (scrolled_window), view);
    gtk_container_add (GTK_CONTAINER(popup_window), scrolled_window);

    gtk_widget_show_all (popup_window);

	char* json = NULL;
	jsmntok_t* json_toks = NULL;
	size_t json_size = 0;
    if (gf_wordnet_lookup(word,&json,&json_toks,&json_size)) {
		populate_suggestions_list(store, json,json_toks,json_size);
		free(json);
		free(json_toks);
	}

	gtk_widget_grab_focus (view);

    /* The tree view has acquired its own reference to the
     *  model, so we can drop ours. That way the model will
     *  be freed automatically when the tree view is destroyed */

    g_object_unref (store);
    g_free(word);
}

static gboolean kb_activate_cb(GeanyKeyBinding *key, guint key_id, gpointer user_data)
{
	item_activate_cb(NULL, user_data);
	return TRUE;
}

static gboolean gf_wordnet_init(GeanyPlugin *plugin, gpointer pdata)
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
