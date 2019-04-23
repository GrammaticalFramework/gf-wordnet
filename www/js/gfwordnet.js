gfwordnet = {}

gfwordnet.grammar_url = "http://cloud.grammaticalframework.org/robust/Parse.pgf"

gfwordnet.grammar_call=function(querystring,cont,errcont) {
    http_get_json(gfwordnet.grammar_url+querystring,cont,errcont)
}

gfwordnet.sense_url = "http://www.grammaticalframework.org/~krasimir/SenseService.fcgi"

gfwordnet.sense_call=function(querystring,cont,errcont) {
    http_get_json(gfwordnet.sense_url+querystring,cont,errcont)
}

gfwordnet.initialize = function () {
	this.lex_ids   = {};
	this.can_check = window.location.href.endsWith("?can_check");
	this.selection = {};
	this.popup     = null;
}

gfwordnet.search = function (selection, input, result) {
	if (input == "")
		return;

	function errcont(text,code) { }
	function extract_linearization(lins) {
		for (var i in lins) {
			var lin   = lins[i];
			var texts = []
			if (gfwordnet.can_check) {
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
	function extract_senses(senses) {
		var index = 1;
		for (var i in senses) {
			result.appendChild(tr(node("td",{colspan: 2 + selection.langs_list.length},[text(index+". "+senses[i].gloss)]))); index++;
			for (var lex_id in senses[i].lex_ids) {
				gfwordnet.lex_ids[lex_id] = senses[i].lex_ids[lex_id];
				gfwordnet.lex_ids[lex_id].synonyms = senses[i].synset;

				var icon;
				var row = this[lex_id];

				var checked = true;
				for (var lang in gfwordnet.selection.langs) {
					if (!(lang in gfwordnet.lex_ids[lex_id].lex_defs)) {
						checked = false;
					} else if (!gfwordnet.lex_ids[lex_id].lex_defs[lang][1]) {
						checked = false;
						var td = row[gfwordnet.selection.langs[lang].index];
						td.classList.add("unchecked");
						if (gfwordnet.can_check)
							td.addEventListener("mouseover", gfwordnet.onmouseover_cell, false);
					}
				}

				if (!checked) {
					icon = img("unchecked.png");
				} else {
					icon = node("img", {src: "checked_plus.png", onclick: "gfwordnet.onclick_minus(event,this)"});
				}
				row[0].insertBefore(icon, row[0].firstChild);
				result.appendChild(tr(row));
			}

			for (var syn_id in senses[i].synset) {
				if (!(syn_id in gfwordnet.lex_ids)) {
					gfwordnet.lex_ids[syn_id] = {
						lex_defs: senses[i].synset[syn_id],
						synonyms: senses[i].synset
					};
				}
			}
		}
	}
	function extract_search(lemmas) {
		gfwordnet.lex_ids = {};
		clear(result);

		var rows        = {};
		var lexical_ids = ""
		
		var row = [th(text("Abstract"))];
		for (var lang in selection.langs_list) {
			row.push(th(text(selection.langs[selection.langs_list[lang]].name)));
		}
		row.push(node("th",{style: "width: 10px; font-style: italic"},[text("f")]));
		result.appendChild(tr(row));

		var min = Number.MAX_VALUE;
		var max = Number.MIN_VALUE;
		for (var i in lemmas) {
			var prob = Math.exp(-lemmas[i].prob);
			if (min > prob) min = prob;
			if (max < prob) max = prob;
		}
		var scale = Math.min(5/max,3/min);

		for (var i in lemmas) {
			var lemma = lemmas[i].lemma;
			if (!(lemma in rows)) {
				var row = [node("td",{onclick: "gfwordnet.onclick_cell(this)"},[text(lemma)])];
				for (var lang in selection.langs) {
					row.push(node("td",{onclick: "gfwordnet.onclick_cell(this)"},[]));
				}
				var rank_bar = node("td",{style: "white-space: nowrap"});
				var rank = Math.round(Math.exp(-lemmas[i].prob)*scale);
				while (rank > 0) {
					rank_bar.appendChild(div_class("bar",[]));
					rank--;
				}
				row.push(rank_bar);
				rows[lemma] = row;
				lexical_ids = lexical_ids+" "+lemma;

				var cmd = gfwordnet.can_check ? "c-linearizeAll" : "c-linearize";
				gfwordnet.grammar_call("?command="+cmd+"&to="+selection.langs_list.join("%20")+"&tree="+encodeURIComponent(lemma),bind(extract_linearization,row),errcont);
			}
		}
		gfwordnet.sense_call("?lexical_ids="+encodeURIComponent(lexical_ids),bind(extract_senses,rows),errcont);
    }

	this.selection = selection;
	gfwordnet.grammar_call("?command=c-lookupmorpho&input="+encodeURIComponent(input)+"&from="+selection.current,extract_search,errcont);
}
gfwordnet.init_wordcloud = function(canvas, context_size_range) {
	var context      = this.lex_ids[canvas.lex_id].context;
	var context_size = parseInt(context_size_range.value);

	var min = Number.MAX_VALUE;
	var max = Number.MIN_VALUE;
	for (var i = 0; i < context_size; i++) {
		if (max < context[i].prob)
			max = context[i].prob;
		if (min > context[i].prob)
			min = context[i].prob;
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
	var icon = cell.parentNode.firstChild.firstChild;
	if (icon.src.endsWith("unchecked.png"))
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
		cell.parentNode.parentNode.insertBefore(tr(node("td",{colspan: 2 + gfwordnet.selection.langs_list.length, class: "details"},[details])), cell.parentNode.nextSibling);

		cell.parentNode.firstChild.firstChild.src = "checked_minus.png";
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
		siblings[i].className = "unselected"
	}
	cell.className = "selected";

	var lex_id = cell.parentNode.firstChild.firstChild.nextSibling.textContent;

	if (index == 0) {
		var lex_def = this.lex_ids[lex_id];

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
					if (lang in lex_def.synonyms[synonym]) {
						if (!lex_def.synonyms[synonym][lang][1]) {
							cell.classList.add("unchecked");
							if (gfwordnet.can_check)
								cell.addEventListener("mouseover", gfwordnet.onmouseover_cell, false);
							checked = false;
						}
					} else {
						checked = false;
					}
					row.push(cell);
				}
				row.splice(0,0,td([img(checked ? "checked.png" : "unchecked.png"), text(synonym)]));

				gfwordnet.grammar_call("?command=c-linearize&to="+gfwordnet.selection.langs_list.join("%20")+"&tree="+encodeURIComponent(synonym),bind(extract_linearization_synonym,row),errcont);
				result.appendChild(tr(row));
			}
			details.appendChild(result);
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
	if (!event.target.classList.contains("unchecked"))
		return;

	if (gfwordnet.popup != null) {
		if (gfwordnet.popup.parentNode == event.target)
			return;

		gfwordnet.popup.parentNode.removeChild(gfwordnet.popup);
	}
	gfwordnet.popup = div_class("floating",[node("img",{src: "validate.png", onclick: "gfwordnet.onclick_check(this.parentNode.parentNode)"},[])/*,node("img",{src: "pen.png", onclick: "gfwordnet.onclick_check(this.parentNode.parentNode)"},[])*/]);
	event.target.appendChild(gfwordnet.popup);
}
gfwordnet.onclick_minus = function (event, icon) {
	if (!icon.src.endsWith("checked_minus.png"))
		return;

	event.stopPropagation();

	var row = icon.parentNode.parentNode;
	var siblings = row.children;
	for (var i = 0; i < siblings.length; i++) {
		siblings[i].className = "unselected"
	}
	row.parentNode.removeChild(row.nextSibling);
	
	icon.src = "checked_plus.png"
}
gfwordnet.onclick_check = function (cell) {
	var index = -1;
	var node  = cell;
    while ((node = node.previousElementSibling)) {
        index++;
    }

	var lex_id  = cell.parentNode.firstChild.firstChild.nextSibling.textContent;
	var lang    = gfwordnet.selection.langs_list[index];
	var def     = gfwordnet.lex_ids[lex_id].lex_defs[lang];

	function errcont(text,code) { }
	function extract_confirm() {
		def[1] = true;
		cell.removeEventListener("mouseover", gfwordnet.onmouseover_cell);
		cell.classList.remove("unchecked");
		gfwordnet.popup.parentNode.removeChild(gfwordnet.popup);
		gfwordnet.popup = null;

		var checked = true;
		for (var lang in gfwordnet.selection.langs) {
			if (!(lang in gfwordnet.lex_ids[lex_id].lex_defs) || !gfwordnet.lex_ids[lex_id].lex_defs[lang][1]) {
				checked = false;
				break;
			}
		}

		if (checked) {
			var img = cell.parentNode.firstChild.firstChild;
			img.src = "checked_plus.png";
			img.onclick = function(event) { gfwordnet.onclick_minus(event, this) };
		}
	}

	gfwordnet.sense_call("?check_id="+encodeURIComponent(lex_id)+"&lang="+encodeURIComponent(lang)+"&def="+encodeURIComponent(def[0]),extract_confirm,errcont);
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
