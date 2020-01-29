gfwordnet = {}

gfwordnet.languages = ["ParseBul", "ParseCat", "ParseChi"
                      ,"ParseDut", "ParseEng", "ParseEst"
                      ,"ParseFin", "ParseIta", "ParsePor"
                      ,"ParseSlv", "ParseSpa", "ParseSwe"
                      ,"ParseTha", "ParseTur"];

(function(){
	var url = new URL(window.location.href);

	gfwordnet.lex_ids     = {};
	gfwordnet.user        = null;
	gfwordnet.author      = null;
	gfwordnet.token       = null;
	gfwordnet.can_select  = url.searchParams.get("can_select") != null;
	gfwordnet.selection   = null;
	gfwordnet.popup       = null;
	gfwordnet.commit_link = null;

	var scripts= document.getElementsByTagName('script');
	var path= scripts[scripts.length-1].src.split('?')[0];      // remove any ?query
	gfwordnet.script_url = path.split('/').slice(0, -1).join('/')+'/';
})();

gfwordnet.grammar_url = "https://cloud.grammaticalframework.org/robust/Parse.pgf"

gfwordnet.grammar_call=function(querystring,cont,errcont) {
    http_get_json(gfwordnet.grammar_url+querystring,cont,errcont)
}

gfwordnet.sense_url = gfwordnet.script_url + "../SenseService.fcgi"

gfwordnet.sense_call=function(querystring,cont,errcont) {
    http_get_json(gfwordnet.sense_url+querystring,cont,errcont)
}

gfwordnet.content_url = gfwordnet.script_url + "../ContentService.fcgi"

gfwordnet.content_call=function(querystring,cont,errcont) {
    http_get_json(gfwordnet.content_url+querystring,cont,errcont)
}

gfwordnet.shell_url = "https://cloud.grammaticalframework.org/gfshell"

gfwordnet.shell_call=function(querystring,cont,errcont) {
	ajax_http_get(gfwordnet.shell_url+querystring,cont,errcont)
}

gfwordnet.set_user = function(user,author,token,count,result,commit_link) {
	var thead = result.getElementsByTagName("THEAD")[0];
	thead.innerHTML = "";
	
	var tbody = result.getElementsByTagName("TBODY")[0];
	tbody.innerHTML = "";

	var tfoot = result.getElementsByTagName("TFOOT")[0];
	tfoot.innerHTML = "";

	this.lex_ids = {};
	this.user        = user;
	this.author      = author;
	this.token       = token;
	this.selection   = null;
	this.popup       = null;
	this.commit_link = commit_link;
	
	this.update_count(count);
}

gfwordnet.populate_domains = function (domains, domain_listener) {
	function errcont(text,code) { }
	function extract_domains(res) {
		var thead = domains.getElementsByTagName("THEAD")[0];
		thead.appendChild(tr(th(text("Domains"))));

		var tbody = domains.getElementsByTagName("TBODY")[0];
		var trow  = null;
		for (var i = 0; i < res.length; i++) {
			var checkbox = node("input", {type: "checkbox"});
			checkbox.addEventListener("change", domain_listener);
			if (trow == null || trow.childElementCount >= 5) {
				trow = tr([]);
				tbody.appendChild(trow);
			}
			trow.appendChild(td([checkbox,text(res[i])]));
		}
	}
	gfwordnet.sense_call("?list_domains",bind(extract_domains),errcont);
}

gfwordnet.get_selected_domains = function(domains) {
	var items        = domains.querySelectorAll("input");
	var domains_map  = {};
	for (var i=0; i<items.length; i++) {
		if (items[i].checked) {
			var domain = items[i].nextSibling.textContent;
			domains_map[domain] = null;
		}
	}
	return domains_map;
}

gfwordnet.render_rows = function(result,selection,new_selection,lemmas) {
	var rows        = {};

	var result_thead = result.getElementsByTagName("THEAD")[0];

	if (new_selection) {
		clear(result_thead);

		var row = [th(text("Abstract"))];
		for (var lang in selection.langs_list) {
			row.push(th(text(selection.langs[selection.langs_list[lang]].name)));
		}
		row.push(node("th",{style: "width: 10px; font-style: italic"},[text("f")]));
		if (gfwordnet.can_select) {
			row.push(th([]));
		}
		result_thead.appendChild(tr(row));
	}
	
	if (result_thead.firstElementChild != null &&
		result_thead.firstElementChild.nextElementSibling != null)
			result_thead.removeChild(result_thead.firstElementChild.nextElementSibling);

	var result_tbody = result.getElementsByTagName("TBODY")[0];
	clear(result_tbody);

	var editors = document.body.getElementsByClassName("editor");
	for (var i=0; i < editors.length; i++) {
		document.body.removeChild(editors[i]);
	}

	var min = Number.MAX_VALUE;
	var max = Number.MIN_VALUE;
	for (var i in lemmas) {
		var prob = Math.exp(-lemmas[i].prob);
		if (min > prob) min = prob;
		if (max < prob) max = prob;
	}
	var scale = Math.min(5/max,3/min);

	function errcont(text,code) { }
	function extract_linearization(lins) {
		for (var i in lins) {
			var lin   = lins[i];
			var texts = []
			if (gfwordnet.user != null) {
				for (var i in lin.texts) {
					if (!lin.texts[i].startsWith("["))
						texts.push(lin.texts[i]);
				}
			} else {
				if (!lin.text.startsWith("["))
					texts.push(lin.text);
			}
			this[selection.langs[lin.to].index].appendChild(text(texts.join(", ")));
		}
	}

	for (var i in lemmas) {
		var lemma = lemmas[i].lemma;
		if (!(lemma in rows)) {
			var row = [node("td",{onclick: "gfwordnet.onclick_cell(this)"},[text(lemma)])];
			for (var lang in selection.langs) {
				var cell = node("td",{onclick: "gfwordnet.onclick_cell(this)"},[]);
				if (gfwordnet.user != null)
					cell.addEventListener("mouseover", gfwordnet.onmouseover_cell, false);
				row.push(cell);
			}
			var rank_bar = node("td",{style: "white-space: nowrap"});
			var rank = Math.round(Math.exp(-lemmas[i].prob)*scale);
			while (rank > 0) {
				rank_bar.appendChild(div_class("bar",[]));
				rank--;
			}
			row.push(rank_bar);
			if (gfwordnet.can_select) {
				row.push(td([node("button",{style: "float: right", onclick: "gfwordnet.onclick_select(this.parentNode.parentNode)"},[text("\u25BC")])]));
			}
			rows[lemma] = row;

			var cmd = (gfwordnet.user != null) ? "c-linearizeAll" : "c-linearize";
			gfwordnet.grammar_call("?command="+cmd+"&to="+selection.langs_list.join("%20")+"&tree="+encodeURIComponent(lemma),bind(extract_linearization,row),errcont);
		}
	}
	return rows;
}

gfwordnet.render_senses = function(ctxt,selection,result,domains,senses) {
	var index = 1;

	var colspan = selection.langs_list.length + 2 + (gfwordnet.can_select ? 1 : 0);

	var result_thead  = result.getElementsByTagName("THEAD")[0];
	if (senses.total > senses.retrieved) {
		result_thead.appendChild(tr([node("th",{colspan: colspan},[text(senses.retrieved+" out of "+senses.total)])]));
	}

	var result_tbody  = result.getElementsByTagName("TBODY")[0];
	var domains_tbody = (domains != null) ? domains.getElementsByTagName("TBODY")[0] : null;

	var domains_row = null;

	for (var i in senses.result) {
		result_tbody.appendChild(tr(node("td",{colspan: colspan},[text(index+". "+senses.result[i].gloss)])));
		index++;

		for (var lex_id in senses.result[i].lex_ids) {
			if (!(lex_id in gfwordnet.lex_ids && gfwordnet.lex_ids[lex_id].match)) {
				gfwordnet.lex_ids[lex_id] = senses.result[i].lex_ids[lex_id];
				gfwordnet.lex_ids[lex_id].synonyms = senses.result[i].lex_ids;
			}

			if (!senses.result[i].lex_ids[lex_id].match)
				continue;

			var icon;
			var row = ctxt.rows[lex_id];

			var checked = true;
			for (var lang in gfwordnet.selection.langs) {
				if (!(lang in gfwordnet.lex_ids[lex_id].status)) {
					checked = false;
				} else if (gfwordnet.lex_ids[lex_id].status[lang] != "checked") {
					checked = false;
					var cell = row[gfwordnet.selection.langs[lang].index];
					cell.classList.add(gfwordnet.lex_ids[lex_id].status[lang]);
				}
			}

			icon = node("img", {src: gfwordnet.script_url+(checked ? "../checked_plus.png" : "../unchecked_plus.png"),
								onclick: "gfwordnet.onclick_minus(event,this)"});
			row[0].insertBefore(icon, row[0].firstChild);
			result_tbody.appendChild(node("tr",{"data-lex-id": lex_id},row));

			if (domains != null) {
				for (var j in gfwordnet.lex_ids[lex_id].domains) {
					var domain = gfwordnet.lex_ids[lex_id].domains[j];
					if (ctxt.domains_map[domain] == null) {
						if (domains_row == null || domains_row.childElementCount >= 5) {
							domains_row = tr([]);
							domains_tbody.appendChild(domains_row);
						}
						var checkbox = node("input", {type: "checkbox"});
						checkbox.checked = domain in ctxt.domains_map;
						checkbox.addEventListener("change", ctxt.domain_listener);
						var cell = td([checkbox,text(domain)]);
						ctxt.domains_map[domain] = cell;
						domains_row.appendChild(cell);
					}
				}
			}
		}
	}

	var tfoot = node("tfoot", {});
	result.appendChild(tfoot);
}
function domain_search_listener() {
	this.domains_map = gfwordnet.get_selected_domains(domains);
	var new_senses  = {total:     this.senses.total
					  ,retrieved: 0
					  ,result:    []
					  };

	for (var i in this.senses.result) {
		var sense = this.senses.result[i];

		var new_sense = Object.create(sense);
		new_sense.lex_ids = {}

		for (var lex_id in sense.lex_ids) {
			var is_included = true;
			for (var domain in this.domains_map) {
				if (sense.lex_ids[lex_id].domains == null ||
					!sense.lex_ids[lex_id].domains.includes(domain)) {
					is_included = false;
					break;
				}
			}
			if (is_included) {
				new_sense.lex_ids[lex_id] = sense.lex_ids[lex_id];
				new_senses.retrieved++;
			}
		}
		
		if (Object.keys(new_sense.lex_ids).length > 0) {
			new_senses.result.push(new_sense);
		}
	}
	new_senses.total = new_senses.retrieved;

	var result_tbody = result.getElementsByTagName("TBODY")[0];
	clear(result_tbody);

	var domains_tbody = domains.getElementsByTagName("TBODY")[0];
	clear(domains_tbody);

	for (var lex_id in this.rows) {
		var row  = this.rows[lex_id];
		var icon = row[0].firstElementChild;
		if (icon != null) {
			icon.parentElement.removeChild(icon);
		}
		for (var i in row) {
			row[i].removeAttribute("class");
		}
	}

	gfwordnet.render_senses(this,gfwordnet.selection,result,domains,new_senses);
}

gfwordnet.search = function (selection, input, domains, result, domain_listener) {
	var domains_map = this.get_selected_domains(domains);

	if ((input == "" || input == null) && Object.keys(domains_map).length === 0) {
		this.selection = null;

		var result_thead = result.getElementsByTagName("THEAD")[0];
		clear(result_thead);

		var result_tbody = result.getElementsByTagName("TBODY")[0];
		clear(result_tbody);

		var domains_thead = domains.getElementsByTagName("THEAD")[0];
		clear(domains_thead);

		var domains_tbody = domains.getElementsByTagName("TBODY")[0];
		clear(domains_tbody);

		this.populate_domains(domains, domain_listener);
		return;
	}

	function errcont(text,code) { }
	function extract_search(lemmas) {
		gfwordnet.lex_ids = Object.create(gfwordnet.selection.lex_ids);

		var obj = {rows: gfwordnet.render_rows(result, selection, new_selection, lemmas), domains_map: {}};
		obj.domain_listener = bind(domain_search_listener,obj);
		var lexical_ids = "";
		for (lemma in obj.rows) {
			lexical_ids = lexical_ids+" "+lemma;
		}
		var helper = function (senses) {
			this.senses = senses; // save the result to be used for filtering
			gfwordnet.render_senses(this,selection,result,domains,senses);
		}
		gfwordnet.sense_call("?lexical_ids="+encodeURIComponent(lexical_ids),bind(helper,obj),errcont);
	}
	function extract_domains(senses) {
		var lemmas = [];
		for (var i in senses.result) {
			for (var lemma in senses.result[i].lex_ids) {
				lemmas.push({lemma: lemma, prob: Infinity});
			}
		}
		gfwordnet.lex_ids = Object.create(gfwordnet.selection.lex_ids);
		var obj = {rows: gfwordnet.render_rows(result, selection, new_selection, lemmas),
			       domain_listener: domain_listener,
			       domains_map: domains_map};
		gfwordnet.render_senses(obj,selection,result,domains,senses);
	}

	var new_selection = this.selection == null || !selection.isEqual(this.selection);
	this.selection = { langs_list: selection.langs_list
		             , langs:      selection.langs
		             , lex_ids:    this.selection==null ? {} : this.selection.lex_ids
		             };

	var domains_thead = domains.getElementsByTagName("THEAD")[0];
	clear(domains_thead);

	var domains_tbody = domains.getElementsByTagName("TBODY")[0];
	clear(domains_tbody);

	if (input == "" || input == null) {
		var domain_query = "";
		for (var domain in domains_map) {
			if (domain_query != "")
				domain_query = domain_query+"&";
			domain_query = domain_query + 
				           "domain=" + encodeURIComponent(domain);
		}
		gfwordnet.sense_call("?"+domain_query,extract_domains,errcont);
	} else {
		gfwordnet.grammar_call("?command=c-lookupmorpho&input="+encodeURIComponent(input)+"&from="+selection.current,extract_search,errcont);
	}

	if (new_selection) {
		var tfoot = result.getElementsByTagName("TFOOT")[0];
		clear(tfoot);
	}
}
gfwordnet.init_wordcloud = function(canvas, context_size_range) {
	var context      = this.lex_ids[canvas.lex_id].context;
	var context_size = parseInt(context_size_range.value);
	if (context_size > context.length)
		context_size = context.length;

	var min = Number.MAX_VALUE;
	var max = Number.MIN_VALUE;
	for (var i = 0; i < context_size; i++) {
		if ("prob" in context[i]) {
			if (max < context[i].prob)
				max = context[i].prob;
			if (min > context[i].prob)
				min = context[i].prob;
		}
	}
	var popup = canvas.parentNode.className == "popup";
	var fontSize = parseInt(window.getComputedStyle(document.getElementsByTagName("body")[0]).getPropertyValue('font-size'));
	var scale = fontSize*Math.min((popup ? 8 : 2)/max,(popup ? 2 : 0.5)/min);
	var list = [];
	for (var i = 0; i < context_size; i++) {
		var size = context[i].prob*scale;
		if (size > 1) {
			if (size < 9)
				size = 9;
			if ("head" in context[i])
				list.push([context[i].head,size,"orange"]);
			else
				list.push([context[i].mod, size,"turquoise"]);
		}
	}
	if (list.length > 1) {
		WordCloud(canvas,{list: list, shuffle: false});
	}
}
gfwordnet.init_embedding = function(canvas, context_size_range) {
	var relations = this.lex_ids[canvas.lex_id].relations;
	var context_size = parseInt(context_size_range.value);

	var tsne = new tsnejs.tSNE({}); // create a tSNE instance

	var dists = [];
	for (var i = 0; i < context_size; i++) {
		dists.push(relations[i].vec);
	}
	tsne.initDataDist(dists);

	for(var k = 0; k < 500; k++) {
	  tsne.step(); // every time you call this, solution gets better
	}

	var points = tsne.getSolution(); // Y is an array of 2-D points that you can plot
	
	var minx = Number.MAX_VALUE;
	var maxx = Number.MIN_VALUE;
	var miny = Number.MAX_VALUE;
	var maxy = Number.MIN_VALUE;
	for (var i in points) {
		var point = points[i];
		if (point[0] < minx) minx = point[0];
		if (point[0] > maxx) maxx = point[0];
		if (point[1] < miny) miny = point[1];
		if (point[1] > maxy) maxy = point[1];
	}
	var scalex = (canvas.width -60)/(maxx-minx);
	var scaley = (canvas.height-60)/(maxy-miny);
	var scale  = Math.min(scalex,scaley);

	var popup = canvas.parentNode.className == "popup";
	var fontSize = window.getComputedStyle(document.getElementsByTagName("body")[0]).getPropertyValue('font-size');

	var ctx = canvas.getContext("2d");
	ctx.clearRect(0, 0, canvas.width, canvas.height);
	ctx.font = fontSize + " Ariel";
	for (var i = 0; i < context_size; i++) {
		var point = points[i];
		var fun   = relations[i].fun;
		if (fun == canvas.lex_id)
			ctx.fillStyle = '#ff0000';
		ctx.fillText(fun, (point[0]-minx)*scale, (point[1]-minx)*scale);
		if (fun == canvas.lex_id)
			ctx.fillStyle = '#000000';
	}
}
gfwordnet.init_canvas = function (tab,canvas,context_size_range) {
	if (tab.innerHTML == "Context") {
		gfwordnet.init_wordcloud(canvas,context_size_range);
	} else if (tab.innerHTML == "Related") {
		gfwordnet.init_embedding(canvas,context_size_range);
	}
}
gfwordnet.onclick_cell = function (cell) {
	if (cell.innerHTML == "")
		return;

	function errcont(text,code) { }
	function extract_context(res) {
		gfwordnet.lex_ids[this.lex_id].context   = res.context;
		gfwordnet.lex_ids[this.lex_id].relations = res.relations;

		var context_size_range = node("input", {id: "context_size", type: "range", min: 1, max: 200, value: 100, onchange: "gfwordnet.onchange_context_size(this)"});
		var tabs = node("table",{class: "header-tabs"},[
				 tr([td(node("h1",{class: "selected",   onclick: "gfwordnet.onclick_tab(this)"},[text("Context")])),
					 td(node("h1",{class: "unselected", onclick: "gfwordnet.onclick_tab(this)"},[text("Related")])),
					 td(context_size_range)
					])]);
		this.popup.appendChild(tabs);

		var canvas = node("canvas", {width: 10, height: 10, onclick: "gfwordnet.onclick_canvas(this)"}, []);
		canvas.lex_id = this.lex_id;
		this.popup.appendChild(canvas);

		gfwordnet.init_wordcloud(canvas,context_size_range);
	}
	var bind_state = true;
	function taggedBrackets(brackets) {
		var tags = [];
		for (var i in brackets) {
			if ("bind" in brackets[i])
				bind_state = brackets[i].bind;
			else {
				if (!bind_state) {
					tags.push(text(" "));
					bind_state = true;
				}

				if ("token" in brackets[i]) {
					tags.push(text(brackets[i].token));
					bind_state = false;
				} else {
					tags.push(node("span", {"fid": brackets[i].fid,
											"fun": brackets[i].fun,
											"onclick": "gfwordnet.onclick_bracket(event, this)"},
								   taggedBrackets(brackets[i].children)));
				}
			}
		}
		return tags;
	}
	function extract_linearization(lins) {
		var rows = []
		for (var i in lins) {
			var lin = lins[i];
			bind_state = true;
			rows.push(tr([th(text(gfwordnet.selection.langs[lin.to].name)), td(taggedBrackets(lin.brackets))]));
		}
		this.parentNode.insertBefore(node("table",{class: "result"},rows), this.nextSibling);
	}
	function extract_linearization_synonym(lins) {
		for (var i in lins) {
			var lin = lins[i];
			if (!lin.text.startsWith("["))
				this[gfwordnet.selection.langs[lin.to].index].appendChild(text(lin.text));
		}
	}
	function extract_linearization_morpho(lins) {
		this.innerHTML = lins[0].text;
	}

	var details = null;
	if (cell.parentNode.nextSibling == null ||
	    cell.parentNode.nextSibling.firstChild == null ||
	    cell.parentNode.nextSibling.firstChild.className != "details") {
		details = node("div", {}, []);
		cell.parentNode.parentNode.insertBefore(tr(node("td",{colspan: 2 + gfwordnet.selection.langs_list.length + (gfwordnet.can_select ? 1 : 0), class: "details"},[details])), cell.parentNode.nextSibling);

		var icon = cell.parentNode.firstChild.firstChild;
		icon.src = icon.src.substring(0,icon.src.length-8)+"minus.png"
	} else {
		details = cell.parentNode.nextSibling.firstChild.firstChild;
		clear(details);
	}

	var index = -1;
	var siblings = cell.parentNode.children;
	for (var i = 0; i < siblings.length; i++) {
		if (siblings[i] == cell) {
			index = i;
		}
		siblings[i].classList.remove("selected");
		siblings[i].classList.add("unselected");
	}
	cell.classList.add("selected");

	var lex_id = cell.parentNode.getAttribute("data-lex-id");

	if (index == 0) {
		var lex_def = this.lex_ids[lex_id];

		for (var i in lex_def.images) {
			var path = lex_def.images[i][1].split("/");
			var name = path[path.length-1];
			path.splice(0,0,"https://upload.wikimedia.org/wikipedia");
			var a = node("a", {href: "https://www.wikipedia.org/wiki/"+lex_def.images[i][0], target: "wiki_link"}, []);
			details.appendChild(a);
			if (name.endsWith(".svg")) {
				a.appendChild(
				  node("img",{"class": "thumbnail"
					         ,style: "width: 300px"
							 ,src: path.join("/")}));
			} else {
				path.splice(2,0,"thumb");
				path.push("300px-"+name);
				a.appendChild(
				  node("img",{"class": "thumbnail"
							 ,src: path.join("/")}));
			}
		}

		var row = [];
		if (Object.keys(lex_def.synonyms).length > 1) {
			details.appendChild(node("h1",{},[text("Synonyms")]));
			var result = node("table",{class: "result"},[]);
			var row = [th(text("Abstract"))]
			for (var lang in gfwordnet.selection.langs_list) {
				row.push(th(text(gfwordnet.selection.langs[gfwordnet.selection.langs_list[lang]].name)));
			}
			result.appendChild(tr(row));
			for (var synonym in lex_def.synonyms) {
				if (synonym == lex_id)
					continue;

				var row = []
				var checked = true;
				for (var i in gfwordnet.selection.langs_list) {
					var lang = gfwordnet.selection.langs_list[i];
					var cell = td([]);
					if (lang in lex_def.synonyms[synonym].status) {
						if (lex_def.synonyms[synonym].status[lang] != "checked") {
							cell.classList.add(lex_def.synonyms[synonym].status[lang]);
							if (gfwordnet.user != null)
								cell.addEventListener("mouseover", gfwordnet.onmouseover_cell, false);
							checked = false;
						}
					} else {
						checked = false;
					}
					row.push(cell);
				}
				row.splice(0,0,td([img(gfwordnet.script_url+(checked ? "../checked.png" : "../unchecked.png")), text(synonym)]));

				gfwordnet.grammar_call("?command=c-linearize&to="+gfwordnet.selection.langs_list.join("%20")+"&tree="+encodeURIComponent(synonym),bind(extract_linearization_synonym,row),errcont);
				result.appendChild(node("tr",{"data-lex-id": synonym},row));
			}
			details.appendChild(result);
		}
		if (lex_def.domains.length > 0) {
			var header = node("h1",{},[text("Domains")]);
			details.appendChild(header);

			var row = [];
			for (var j in lex_def.domains) {
				row.push(td([text(lex_def.domains[j])]));
			}
			details.appendChild(node("table",{class: "selectors"},[tr(row)]));
		}
		if (lex_def.examples.length > 0) {
			var header = node("h1",{},[text("Examples")]);
			details.appendChild(header);
			for (var i in lex_def.examples) {
				gfwordnet.grammar_call("?command=c-bracketedLinearize&to="+gfwordnet.selection.langs_list.join("%20")+"&tree="+encodeURIComponent(lex_def.examples[i]),bind(extract_linearization,header),errcont);
			}
		}
		if (lex_def.secondary_examples.length > 0) {
			var header = node("h1",{},[text("Secondary Examples")]);
			details.appendChild(header);
			for (var i in lex_def.secondary_examples) {
				gfwordnet.grammar_call("?command=c-bracketedLinearize&to="+gfwordnet.selection.langs_list.join("%20")+"&tree="+encodeURIComponent(lex_def.secondary_examples[i]),bind(extract_linearization,header),errcont);
			}
		}
		
		popup = node("div",{},[]);
		details.appendChild(popup);

		if ("context" in lex_def)
			bind(extract_context,{lex_id: lex_id, popup: popup})(lex_def);
		else
			gfwordnet.sense_call("?context_id="+encodeURIComponent(lex_id),bind(extract_context,{lex_id: lex_id, popup: popup}),errcont);	
	} else {
		var s   = lex_id.split("_");
		var cat = s[s.length-1];
		gfwordnet.grammar_call("?command=c-linearize&to="+gfwordnet.selection.langs_list[index-1]+"&tree="+encodeURIComponent("MkDocument (NoDefinition \"\") (Inflection"+cat+" "+lex_id+") \"\""),bind(extract_linearization_morpho,details),errcont);
	}
}
gfwordnet.onmouseover_cell = function(event) {
	if (event.target.tagName != "TD")
		return;
	if (gfwordnet.popup != null) {
		if (gfwordnet.popup.parentNode == event.target)
			return;
		gfwordnet.popup.parentNode.removeChild(gfwordnet.popup);
	}
	var row = [];
	if (event.target.classList.contains("unchecked") || event.target.classList.contains("guessed")) {
		var btn = img(gfwordnet.script_url+"../validate.png");
		btn.addEventListener("click", gfwordnet.onclick_check, false);
		row.push(btn);
	}
	var btn = img(gfwordnet.script_url+"../edit.png");
	btn.addEventListener("click", gfwordnet.onclick_edit, false);
	row.push(btn);
	gfwordnet.popup = div_class("floating",row);
	event.target.appendChild(gfwordnet.popup);
}
gfwordnet.onclick_minus = function (event, icon) {
	if (!icon.src.endsWith("_minus.png"))
		return;

	event.stopPropagation();

	var row = icon.parentNode.parentNode;
	var siblings = row.children;
	for (var i = 0; i < siblings.length; i++) {
		siblings[i].classList.remove("selected");
		siblings[i].classList.add("unselected");
	}
	row.parentNode.removeChild(row.nextSibling);
	
	icon.src = icon.src.substring(0,icon.src.length-9)+"plus.png";
}
gfwordnet.update_cells = function(lex_id,lang) {
	var index  = gfwordnet.selection.langs[lang].index;
	var status = this.lex_ids[lex_id].status[lang];

	var rows = document.querySelectorAll("tr[data-lex-id="+lex_id+"]");
	for (var i=0; i<rows.length; i++) {
		var row  = rows[i];
		var cell = row.childNodes[index];

		cell.classList.remove("unchecked");
		cell.classList.remove("guessed");
		cell.classList.remove("changed");
		if (status != "checked")
			cell.classList.add(status);

		var checked = true;
		for (var lang in gfwordnet.selection.langs) {
			if (!(lang in gfwordnet.lex_ids[lex_id].status) || 
			    gfwordnet.lex_ids[lex_id].status[lang] != "checked") {
				checked = false;
				break;
			}
		}

		if (checked) {
			var icon = row.firstChild.firstChild;
			icon.src = icon.src.endsWith("unchecked_plus.png")  ? "checked_plus.png"  :
			           icon.src.endsWith("unchecked_minus.png") ? "checked_minus.png" :
			                                                      "checked.png"       ;
		}
	}
}
gfwordnet.onclick_check = function (event) {
	event.stopPropagation();

	var cell = event.target.parentNode.parentNode;

	var index = -1;
	var node  = cell;
    while ((node = node.previousElementSibling)) {
        index++;
    }

	var lex_id = cell.parentNode.getAttribute("data-lex-id");
	var lang   = gfwordnet.selection.langs_list[index];

	function errcont(text,code) { }
	function extract_confirm(st) {
		gfwordnet.popup.parentNode.removeChild(gfwordnet.popup);
		gfwordnet.popup = null;

		gfwordnet.lex_ids[lex_id].status[lang] = st[1];
		gfwordnet.update_cells(lex_id,lang);
		gfwordnet.update_count(st[0]);
	}

	gfwordnet.content_call("?user="+gfwordnet.user+"&update_id="+encodeURIComponent(lex_id)+"&lang="+encodeURIComponent(lang),extract_confirm,errcont);
}
gfwordnet.onclick_eval = function(event,editor) {
	var editor = event.target.parentNode.parentNode.parentNode;
	
	var index = -1;
	var cell  = editor.cell;
    while ((cell = cell.previousElementSibling)) {
        index++;
    }

	var lex_id = editor.cell.parentNode.getAttribute("data-lex-id");
	var lang   = gfwordnet.selection.langs_list[index];
	var def    = editor.firstElementChild.firstElementChild.firstElementChild.value;
	var dir    = "/tmp/morpho-"+lang.slice(5);
	var cat    = lex_id.slice(lex_id.lastIndexOf("_")+1);

	function errcont(text,code) { }
	function extract_html(html) {
		var result = editor.childNodes[2].firstElementChild.firstElementChild;
		if (html.includes("<") && html.includes(">")) {
			result.innerHTML = html;
			event.target.nextElementSibling.style.display = "inline-block";
		} else {
			result.innerHTML = "";
			result.appendChild(node("pre", {}, [text(html)]));
			event.target.nextElementSibling.style.display = "none";
		}
	}
	function extract_import(html) {
		if (html != "") {
			extract_html(html);
			return;
		}
		gfwordnet.shell_call("?dir="+dir+"&command=cc%20-one%20"+encodeURIComponent("MkDocument (NoDefinition {s=\"\"}) (Inflection"+cat+" ("+def+")) {s=\"\"}"),extract_html,errcont);
	}
	gfwordnet.shell_call("?dir="+dir+"&command=i+-retain+morpho.gf",extract_import,errcont);
}
gfwordnet.onclick_save = function(event) {
	event.stopPropagation();

	var editor = event.target.parentNode.parentNode.parentNode;

	var index = -1;
	var node  = editor.cell;
    while ((node = node.previousElementSibling)) {
        index++;
    }

	var lex_id = editor.cell.parentNode.getAttribute("data-lex-id");
	var lang   = gfwordnet.selection.langs_list[index];
	var def    = editor.firstElementChild.firstElementChild.firstElementChild.value;

	function errcont(text,code) { }
	function extract_confirm(st) {
		gfwordnet.popup.parentNode.removeChild(gfwordnet.popup);
		gfwordnet.popup = null;

		gfwordnet.lex_ids[lex_id].status[lang] = st[1];
		gfwordnet.update_cells(lex_id,lang);
		
		gfwordnet.update_count(st[0]);
	}

	gfwordnet.content_call("?user="+gfwordnet.user+"&update_id="+encodeURIComponent(lex_id)+"&lang="+encodeURIComponent(lang)+"&def="+encodeURIComponent(def),extract_confirm,errcont);
	document.body.removeChild(editor);
}
gfwordnet.onclick_delete = function(event) {
	var editor = event.target.parentNode.parentNode.parentNode;
	editor.firstElementChild.firstElementChild.firstElementChild.value = "variants {}";
	gfwordnet.onclick_save(event);
}
gfwordnet.onclick_edit = function (event) {
	event.stopPropagation();

	var cell = event.target.parentNode.parentNode;

	var index = -1;
	var n  = cell;
    while ((n = n.previousElementSibling)) {
        index++;
    }

	var lex_id  = cell.parentNode.getAttribute("data-lex-id");
	var lang    = gfwordnet.selection.langs_list[index];

	function errcont(text,code) { }
	function extract_def(def) {
		var textarea = node("textarea", {rows: 4, cols: 50, spellcheck: false},[text(def)]);
		var evalBtn   = node("button", {},[text("Eval")]);
		var saveBtn   = node("button", {style: "display: none"},[text("Save")]);
		var deleteBtn = node("button", {},[text("Delete")]);
		var cancelBtn = node("button", {onclick: "document.body.removeChild(this.parentNode.parentNode.parentNode)"},[text("Cancel")]);
		var editor    = node("table", {"class": "editor"} ,
								[tr(node("td",{colspan:3},[textarea])),
								 tr([td([evalBtn,saveBtn,deleteBtn,cancelBtn])]),
								 tr([td([node("div",{style: "max-height: 300px; overflow:auto"},[])])])]);
		editor.style.top  = event.clientY+"px";
		if (window.matchMedia("(min-resolution: 300dpi)").matches) {
			editor.style.left  = "0px";
			editor.style.width = "100%";
		} else {
			editor.style.left  = event.clientX+"px";
		}

		textarea.addEventListener("keydown", function(event) {
			if (event.keyCode === 13) {
				event.preventDefault();
				evalBtn.click();
			}
		});
		evalBtn.addEventListener("click", gfwordnet.onclick_eval, false);
		saveBtn.addEventListener("click", gfwordnet.onclick_save, false);
		deleteBtn.addEventListener("click", gfwordnet.onclick_delete, false);
		editor.addEventListener('mousedown', gfwordnet.onmove_dialog, true);

		editor.cell = cell;
		document.body.appendChild(editor);
		textarea.focus();
	}

	gfwordnet.content_call("?user="+gfwordnet.user+"&get_id="+encodeURIComponent(lex_id)+"&lang="+encodeURIComponent(lang),extract_def,errcont);
}
gfwordnet.onclick_tab = function (tab) {
	var tr = tab.parentNode.parentNode;
	var td = tr.firstChild;
	while (td != null) {
		if (td.firstChild == tab) {
			td.firstChild.className = "selected";
		} else if (td.firstChild.className == "selected") {
			td.firstChild.className = "unselected";
		}
		td = td.nextSibling;
	}

	var context_size_range = tr.lastElementChild.firstElementChild;
	var canvas = tab.parentNode.parentNode.parentNode.nextSibling;
	gfwordnet.init_canvas(tab,canvas,context_size_range);
}
gfwordnet.onclick_select = function (row) {
	var tbody = row.parentNode;
	var table = tbody.parentNode;
	var tfoot = table.getElementsByTagName("tfoot")[0];

	var prev = row.previousSibling;
	var next = row.nextSibling;

	if (tfoot.childElementCount == 0)
		tfoot.appendChild(tr(node("td",{colspan: 3 + gfwordnet.selection.langs_list.length},
		                        [node("span",{style: "font-size: 20px; font-weight: bold"},[text("Selected")])
		                        ,node("button",{id: "clear_search", style: "float: right", onclick: "gfwordnet.onclick_clear_search(this.parentNode.parentNode.parentNode)"},[text("\u25B2")])
		                        ,node("button",{id: "clear_selection", style: "float: right", onclick: "gfwordnet.onclick_clear_selection(this.parentNode.parentNode.parentNode)"},[text("\u2715")])
		                        ,node("span",{style: "width: 10pt; float: right"},[text("\u00A0")])
		                        ,node("button",{id: "delete", style: "display: none; float: right", onclick: "gfwordnet.onclick_delete_selected_item(this.parentNode.parentNode.parentNode)"},[text("Delete")])
		                        ,node("button",{id: "generalize", style: "display: none; float: right", onclick: "gfwordnet.onclick_generalize_selected_items(this.parentNode.parentNode.parentNode)"},[text("Generalize")])
		                        ])));

	if (prev != null && prev.firstElementChild.hasAttribute("colspan"))
		tbody.removeChild(prev);
	tbody.removeChild(row);
	tfoot.appendChild(row);
	if (next != null && next.firstElementChild.getAttribute("class")=="details") {
		tbody.removeChild(next);
		tfoot.appendChild(next);
	}

	var lex_id = row.firstElementChild.firstElementChild.nextSibling.data;
	this.selection.lex_ids[lex_id] = this.lex_ids[lex_id];
	delete this.lex_ids[lex_id];

	row.lastElementChild.previousElementSibling.innerHTML = "";
	row.lastElementChild.innerHTML = "";
	row.lastElementChild.appendChild(node("input", {type: "checkbox", onclick: "gfwordnet.onclick_selected_item(this.parentNode.parentNode.parentNode)"}));
}
gfwordnet.onclick_selected_item = function (tfoot) {
	var count = 0;
	var row   = tfoot.firstElementChild.nextElementSibling;
	while (row != null) {
		if (!row.firstElementChild.hasAttribute("colspan")) {
			var input = row.lastElementChild.firstElementChild;
			if (input.checked)
				count++;
		}
		row = row.nextElementSibling;
	}

	document.getElementById("generalize").style.display = 
		(count > 1) ? "inline-block" : "none";
	document.getElementById("delete").style.display =
		(count > 0) ? "inline-block" : "none";
}
gfwordnet.onclick_generalize_selected_items = function (tfoot) {
	var lex_ids  = {};
	var selected = [];
	var row      = tfoot.firstElementChild.nextElementSibling;
	while (row != null) {
		if (!row.firstElementChild.hasAttribute("colspan")) {
			var lex_id = row.firstElementChild.firstElementChild.nextSibling.data;
			var input  = row.lastElementChild.firstElementChild;
			if (input.checked) {
				lex_ids[lex_id] = row;
				input.checked   = false;
			}
			selected.push(lex_id);
		}

		row = row.nextElementSibling;
	}

	document.getElementById("generalize").style.display = "none";
	document.getElementById("delete").style.display = "none";

	if (selected.length > 0) {
		function errcont(text,code) { }
		function extract_linearization(lins) {
			for (var i in lins) {
				var lin   = lins[i];
				var texts = []
				if (gfwordnet.user != null) {
					for (var i in lin.texts) {
						if (!lin.texts[i].startsWith("["))
							texts.push(lin.texts[i]);
					}
				} else {
					if (!lin.text.startsWith("["))
						texts.push(lin.text);
				}
				this[gfwordnet.selection.langs[lin.to].index].appendChild(text(texts.join(", ")));
			}
		}
		function extract_generalizations(senses) {
			for (var i in senses) {
				for (var lex_id in senses[i].lex_ids) {
					if (lex_id in gfwordnet.selection.lex_ids)
						continue;

					gfwordnet.selection.lex_ids[lex_id] = senses[i].lex_ids[lex_id];
					gfwordnet.selection.lex_ids[lex_id].synonyms = senses[i].lex_ids;

					var row = [node("td",{onclick: "gfwordnet.onclick_cell(this)"},[text(lex_id)])];

					var checked = true;
					for (var lang in gfwordnet.selection.langs) {
						var cell = node("td",{onclick: "gfwordnet.onclick_cell(this)"},[]);
						if (gfwordnet.user != null)
							cell.addEventListener("mouseover", gfwordnet.onmouseover_cell, false);
						if (!(lang in senses[i].lex_ids[lex_id].status)) {
							checked = false;
						} else if (senses[i].lex_ids[lex_id].status[lang] != "checked") {
							checked = false;
							cell.classList.add(senses[i].lex_ids[lex_id].status[lang]);
						}
						row.push(cell);
					}

					var icon = node("img", {src: gfwordnet.script_url+(checked ? "checked_plus.png" : "unchecked_plus.png"), onclick: "gfwordnet.onclick_minus(event,this)"});
					row[0].insertBefore(icon, row[0].firstChild);

					row.push(node("td",{style: "white-space: nowrap"}));
					row.push(td([node("input", {type: "checkbox", onclick: "gfwordnet.onclick_selected_item(this.parentNode.parentNode.parentNode)"})]));

					tfoot.appendChild(node("tr",{"data-lex-id": lex_id},row));

					var cmd = gfwordnet.user != null ? "c-linearizeAll" : "c-linearize";
					gfwordnet.grammar_call("?command="+cmd+"&to="+gfwordnet.selection.langs_list.join("%20")+"&tree="+encodeURIComponent(lex_id),bind(extract_linearization,row),errcont);
				}
			}
		}

		gfwordnet.sense_call("?generalize_ids="+encodeURIComponent(selected.join(' ')),extract_generalizations,errcont);
	}
}
gfwordnet.onclick_delete_selected_item = function (tfoot) {
	var count = 0;
	var row   = tfoot.firstElementChild.nextElementSibling;
	while (row != null) {
		var next = row.nextElementSibling;

		if (!row.firstElementChild.hasAttribute("colspan")) {
			var input = row.lastElementChild.firstElementChild;
			if (input.checked) {
				tfoot.removeChild(row);
				if (next != null && next.firstElementChild.getAttribute("class")=="details") {
					var nextnext = next;
					tfoot.removeChild(next);
					next = nextnext;
				}
			} else {
				count++;
			}
		} else {
			count++;
		}

		row = next;
	}

	if (count == 0) {
		tfoot.removeChild(tfoot.firstElementChild);
	} else {
		document.getElementById("generalize").style.display = "none";
		document.getElementById("delete").style.display = "none";
	}
}
gfwordnet.onclick_clear_search = function (tfoot) {
	var tbody = tfoot.previousElementSibling;
	tbody.innerHTML = "";
	this.lex_ids = Object.create(this.selection.lex_ids);
}
gfwordnet.onclick_clear_selection = function (tfoot) {
	tfoot.innerHTML = "";
}
gfwordnet.onchange_context_size = function (context_size_range) {
	var tab = null;
	var tr  = context_size_range.parentNode.parentNode;
	var canvas = tr.parentNode.nextElementSibling;
	var td  = tr.firstChild;
	while (td != null) {
		if (td.firstChild.className == "selected") {
			tab = td.firstChild;
			break;
		}
		td = td.nextSibling;
	}
	if (tab == null)
		return;

	if (tab.innerHTML == "Context") {
		gfwordnet.init_wordcloud(canvas,context_size_range);
	} else if (tab.innerHTML == "Related") {
		gfwordnet.init_embedding(canvas,context_size_range);
	}
}
gfwordnet.onclick_canvas = function (canvas) {
	var tab = null;
	var tr  = canvas.parentNode.firstElementChild.firstElementChild;
	var td  = tr.firstChild;
	while (td != null) {
		if (td.firstChild.className == "selected") {
			tab = td.firstChild;
			break;
		}
		td = td.nextSibling;
	}
	if (tab == null)
		return;

	var context_size_range = tr.lastElementChild.firstElementChild;

	if (canvas.parentNode.className == "popup") {
		canvas.parentNode.className = "";
		canvas.width  = canvas.save_width;
		canvas.height = canvas.save_height;
	} else {
		canvas.parentNode.className = "popup";
		canvas.save_width = canvas.width;
		canvas.save_height = canvas.height;
		canvas.width  = canvas.parentNode.offsetWidth;
		canvas.height = canvas.parentNode.offsetHeight-canvas.offsetTop;
	}
	gfwordnet.init_canvas(tab,canvas,context_size_range);
}
gfwordnet.onclick_bracket = function (event, bracket) {
	function errcont(text,code) { }
	function extract_gloss(glosses) {
		var gloss_element = parent.lastElementChild.firstElementChild;
		if (gloss_element.tagName != "TD") {
			gloss_element = node("td",{colspan: 2, style: "max-width: 100px"},[]);
			this.parent.appendChild(tr([gloss_element]));
		}

		if (glosses.length == 0) {
			this.parent.removeChild(this.parent.lastElementChild);
		} else {
			gloss_element.innerHTML = this.lex_id+": "+glosses[0];
		}
	}

	var bracket0 = bracket;
	var parent   = bracket;
	var selected = false;
	while (parent.tagName != "TABLE") {
		if (parent.className=="selected_bracket") {
			selected = true;
		}
		
		if (selected) {
			bracket = parent.parentNode;
			var count   = 0;
			var element = bracket.firstElementChild;
			while (element != null) {
				count++;
				element = element.nextElementSibling;
			}
			if (count > 1)
				selected = false;
		}

		parent = parent.parentNode;
	}

	if (bracket.tagName != "SPAN")
		bracket = null;

	if (bracket == bracket0) {
		var lex_id = bracket.getAttribute("fun");
		if (lex_id != null) {
			gfwordnet.sense_call("?gloss_id="+lex_id,bind(extract_gloss,{parent: parent, lex_id: lex_id}),errcont);
		}
	} else {
		if (parent.lastElementChild.firstElementChild.tagName == "TD") {
			parent.removeChild(parent.lastElementChild);
		}
	}

	function select(element,fid) {
		var child = element.firstElementChild;
		while (child != null) {
			if (fid != null && fid == child.getAttribute("fid"))
				child.className="selected_bracket";
			else
				child.className="";
			select(child,fid);
			child = child.nextElementSibling;
		}
	}
	select(parent,(bracket == null) ? null : bracket.getAttribute("fid"));

	event.stopPropagation();
}

gfwordnet.onmove_dialog = function(event) {
	if (event.target.tagName != "TABLE")
		return;

	var editor = event.target;
	var offset = [event.offsetX,event.offsetY];

	var onmousemove = function(event) {
		event.preventDefault();
		editor.style.left = (event.clientX - offset[0]) + 'px';
		editor.style.top  = (event.clientY - offset[1]) + 'px';
	};
	var onmouseup = function(event) {
		document.removeEventListener('mousemove', onmousemove);
		document.removeEventListener('mouseup',   onmouseup);
	};
	document.addEventListener('mousemove', onmousemove, false);
	document.addEventListener('mouseup', onmouseup, false);
}

gfwordnet.commit = function(commit) {
	var textarea = node("textarea", {rows: 10, cols: 100, spellcheck: false, readonly:true},[]);
	var closeBtn = node("button", {onclick: "document.body.removeChild(this.parentNode.parentNode.parentNode)"},[text("Close")]);
	var editor   = node("table", {"class": "editor"} ,
							[tr(td(textarea))]);

	editor.addEventListener("mousedown", gfwordnet.onmove_dialog);

	document.body.appendChild(editor);

	editor.style.top   = ((window.innerHeight-editor.clientHeight)/2)+"px";
	editor.style.left  = ((window.innerWidth -editor.clientWidth )/2)+"px";

	var xmlHttp = GetXmlHttpObject();
	xmlHttp.onreadystatechange = function() {
		gfwordnet.update_count(0);
		textarea.value = xmlHttp.responseText;
		if (xmlHttp.readyState==4 || xmlHttp.readyState=="complete") {
			textarea.value += "\n\nDone."
			editor.appendChild(tr(td(closeBtn)));
		}
		textarea.scrollTop = textarea.scrollHeight;
	};
	xmlHttp.open("GET", gfwordnet.content_url+"?user="+gfwordnet.user
	                                         +"&author="+gfwordnet.author
	                                         +"&token="+gfwordnet.token
	                                         +"&commit=1",
	                                         true); // true for asynchronous 
	xmlHttp.send(null);
}
gfwordnet.update_count = function(count) {
	if (this.commit_link != null)
		this.commit_link.innerHTML = "Commit ("+count+")";
}

gfwordnet.populate_classes = function (classes, class_div, class_listener) {
	function errcont(text,code) { }
	function extract_classes(res) {
		var thead = classes.getElementsByTagName("THEAD")[0];
		clear(thead);
		thead.appendChild(tr(th(text("Classes"))));

		var tbody = classes.getElementsByTagName("TBODY")[0];
		clear(tbody);
		var trow  = null;
		for (var i = 0; i < res.length; i++) {
			var checkbox = node("input", {type: "checkbox"});
			checkbox.addEventListener("change", class_listener.bind(null,res[i][0]));
			if (trow == null || trow.childElementCount >= 5) {
				trow = tr([]);
				tbody.appendChild(trow);
			}
			trow.appendChild(td([checkbox,text(res[i][1])]));
		}

		clear(class_div);
	}
	gfwordnet.sense_call("?list_top_classes",bind(extract_classes),errcont);
}

gfwordnet.render_class = function (selection, classes, class_div, id, class_listener) {
	this.selection = { langs_list: selection.langs_list
		             , langs:      selection.langs
		             , lex_ids:    this.selection==null ? {} : this.selection.lex_ids
		             };

	function errcont(text,code) { }
	function render_pattern(vars,pattern) {
		var indices = [];
		for (var i = 0; i < vars.length; i++) {
			indices.push({index: pattern.indexOf(vars[i][0]), v: vars[i][0]});
		}
		function compare(a, b) {
			return (a.index - b.index)
		}
		indices.sort(compare);

		var row   = []
		var start = 0;
		for (var i = 0; i < indices.length; i++) {
			var index = indices[i].index;
			if (index < 0)
				continue;

			var chunk = pattern.substring(start,index);
			if (chunk.length > 0)
				row.push(text(chunk));
			row.push(span_class("role",text(indices[i].v)));

			start = index + indices[i].v.length;
		}

		var chunk = pattern.substring(start);
		if (chunk.length > 0)
			row.push(text(chunk));

		return row;
	}
	function render(cls,all_vars) {
		if (cls.vars.length > 0) {
			class_div.appendChild(node("h2",{},[text("Roles")]));
			var rows = [];
			for (var i in cls.vars) {
				var v   = cls.vars[i];
				var row = [span_class("role",text(v[0]))];
				for (var j in v[1]) {
					row.push(span_class("restriction",text(v[1][j])));
				}
				rows.push(li(row));
			}
			class_div.appendChild(node("ul",{},rows));
		}

		gfwordnet.lex_ids = Object.create(gfwordnet.selection.lex_ids);
		class_div.appendChild(node("h2",{},[text("Frames")]));
		var rows = [];
		for (var i in cls.frames) {
			var frame = cls.frames[i];

			var lemmas = [];
			var lexical_ids = "";
			for (var j in frame.fun) {
				lexical_ids = lexical_ids+" "+frame.fun[j];
				lemmas.push({lemma: frame.fun[j], prob: Infinity});
			}

			var result = node("table",{class: "result"},[
			               node("thead",{},[]),
			               node("tbody",{},[]),
			             ]);

			var ctx = {rows:   gfwordnet.render_rows(result,selection,true,lemmas)
				      ,result: result
				      };
			var helper = function (senses) {
				gfwordnet.render_senses(this,selection,this.result,null,senses);
			}

			gfwordnet.sense_call("?lexical_ids="+encodeURIComponent(lexical_ids),bind(helper,ctx),errcont);

			rows.push(li([span_class("pattern",render_pattern(all_vars,frame.pattern)),node("div",{style: "padding: 10px"},[result])]));
		}
		class_div.appendChild(node("ul",{},rows));

		for (var i in cls.subclasses) {
			var subclass = cls.subclasses[i];
			class_div.appendChild(node("table",{class: "selectors"},[tr(td(text(subclass.name)))]));
			render(subclass,all_vars.concat(subclass.vars));
		}
	}
	function extract_class(cls) {
		var thead = classes.getElementsByTagName("THEAD")[0];
		clear(thead);

		var tbody = classes.getElementsByTagName("TBODY")[0];
		clear(tbody);

		var checkbox = node("input", {type: "checkbox"});
		checkbox.checked = true;
		checkbox.addEventListener("change", class_listener);
		tbody.appendChild(tr(td([checkbox,text(cls[0].name)])));

		clear(class_div);
		render(cls[0],[["Verb"]].concat(cls[0].vars));
	}
	gfwordnet.sense_call("?class_id="+id,bind(extract_class),errcont);
}
