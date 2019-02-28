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

	this.langs      = {"ParseBul": {name: "Bulgarian",  index: 1},
		               "ParseEng": {name: "English",    index: 2},
		               "ParseFin": {name: "Finnish",    index: 3},
			           "ParsePor": {name: "Portuguese", index: 4},
			           "ParseSwe": {name: "Swedish",    index: 5}};
	this.langs_list = ["ParseBul", "ParseEng", "ParseFin", "ParsePor", "ParseSwe"];
}

gfwordnet.search = function (from, input, result) {
	if (input == "")
		return;

	function errcont(text,code) { }
	function extract_linearization(lins) {
		for (var i in lins) {
			var lin = lins[i];
			var txt = gfwordnet.can_check ? lin.texts.join(", ") : lin.text;
			this[gfwordnet.langs[lin.to].index].appendChild(text(txt));
		}
	}
	function extract_senses(senses) {
		var index = 1;
		for (var i in senses) {
			result.appendChild(tr(node("td",{colspan: 2 + gfwordnet.langs_list.length + (gfwordnet.can_check ? 1 : 0)},[text(index+". "+senses[i].gloss)]))); index++;
			for (var lex_id in senses[i].lex_ids) {
				gfwordnet.lex_ids[lex_id] = senses[i].lex_ids[lex_id];
				gfwordnet.lex_ids[lex_id].synonyms = senses[i].synset;

				var icon;
				var row = this[lex_id];
				var domains = senses[i].lex_ids[lex_id].domains;
				if (domains.indexOf("unchecked") >= 0) {
					icon = img("unchecked.png");
					if (gfwordnet.can_check)
						row.push(td([node("button",{onclick: "gfwordnet.onclick_check(this)"},[text("Check")])]));
				} else {
					icon = node("img", {src: "checked_plus.png", onclick: "gfwordnet.onclick_minus(event,this)"});
					if (gfwordnet.can_check)
						row.push(td([]));
				}
				row[0].insertBefore(icon, row[0].firstChild);
				result.appendChild(tr(row));
			}
		}
	}
	function extract_search(lemmas) {
		function cell(contents) {
			return node("td",{onclick: "gfwordnet.onclick_cell(this)"},contents);
		}

		gfwordnet.lex_ids = {};
		clear(result);

		var rows        = {};
		var lexical_ids = ""
		
		var row = [th(text("Abstract"))];
		for (var lang in gfwordnet.langs_list) {
			row.push(th(text(gfwordnet.langs[gfwordnet.langs_list[lang]].name)));
		}
		row.push(node("th",{style: "width: 10px; font-style: italic"},[text("f")]));
		if (gfwordnet.can_check)
			row.push(node("th",{style: "width: 10px"},[]));
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
				var row = [cell([text(lemma)])];
				for (var lang in gfwordnet.langs) {
					row.push(cell([]));
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
				gfwordnet.grammar_call("?command="+cmd+"&to="+gfwordnet.langs_list.join("%20")+"&tree="+encodeURIComponent(lemma),bind(extract_linearization,row),errcont);
			}
		}
		gfwordnet.sense_call("?lexical_ids="+encodeURIComponent(lexical_ids),bind(extract_senses,rows),errcont);
    }

	gfwordnet.grammar_call("?command=c-lookupmorpho&input="+encodeURIComponent(input)+"&from="+from,extract_search,errcont);
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
	function taggedBrackets(brackets,bind) {
		var tags = [];
		for (var i in brackets) {
			if (bind)
				bind = false;
			else
				tags.push(text(" "));

			if ("token" in brackets[i]) {
				tags.push(text(brackets[i].token));
			} else if ("bind" in brackets[i])
				bind = brackets[i].bind;
			else {
				tags.push(node("span", {"fid": brackets[i].fid,
										"fun": brackets[i].fun,
										"onclick": "gfwordnet.onclick_bracket(event, this)"},
			                   taggedBrackets(brackets[i].children, bind)));
			}
		}
		return tags;
	}
	function extract_linearization(lins) {
		var rows = []
		for (var i in lins) {
			var lin = lins[i];
			rows.push(tr([th(text(gfwordnet.langs[lin.to].name)), td(taggedBrackets(lin.brackets))]));
		}
		this.parentNode.insertBefore(node("table",{class: "result"},rows), this.nextSibling);
	}
	function extract_linearization_synonym(lins) {
		for (var i in lins) {
			var lin = lins[i];
			this[gfwordnet.langs[lin.to].index].appendChild(text(lin.text));
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
		cell.parentNode.parentNode.insertBefore(tr(node("td",{colspan: 2 + gfwordnet.langs_list.length + (gfwordnet.can_check ? 1 : 0), class: "details"},[details])), cell.parentNode.nextSibling);

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

		popup = node("div",{},[]);
		details.appendChild(popup);

		if ("context" in lex_def)
			bind(extract_context,{lex_id: lex_id, popup: popup})(lex_def);
		else
			gfwordnet.sense_call("?context_id="+encodeURIComponent(lex_id),bind(extract_context,{lex_id: lex_id, popup: popup}),errcont);	

		var row = [];
		if (Object.keys(lex_def.synonyms).length > 1) {
			details.appendChild(node("h1",{},[text("Synonyms")]));
			var result = node("table",{class: "result"},[]);
			var row = [th(text("Abstract"))]
			for (var lang in gfwordnet.langs_list) {
				row.push(th(text(gfwordnet.langs[gfwordnet.langs_list[lang]].name)));
			}
			result.appendChild(tr(row));
			for (var synonym in lex_def.synonyms) {
				if (synonym == lex_id)
					continue;
				var checked = lex_def.synonyms[synonym].indexOf("unchecked") >= 0;
				var row = [td([img(checked ? "unchecked.png" : "checked.png"), text(synonym)])]
				
				for (var lang in gfwordnet.langs_list) {
					row.push(td([]));
				}
				gfwordnet.grammar_call("?command=c-linearize&to="+gfwordnet.langs_list.join("%20")+"&tree="+encodeURIComponent(synonym),bind(extract_linearization_synonym,row),errcont);
				result.appendChild(tr(row));
			}
			details.appendChild(result);
		}
		if (lex_def.examples.length > 0) {
			var header = node("h1",{},[text("Examples")]);
			details.appendChild(header);
			for (var i in lex_def.examples) {
				gfwordnet.grammar_call("?command=c-bracketedLinearize&to="+gfwordnet.langs_list.join("%20")+"&tree="+encodeURIComponent(lex_def.examples[i]),bind(extract_linearization,header),errcont);
			}
		}
		if (lex_def.secondary_examples.length > 0) {
			var header = node("h1",{},[text("Secondary Examples")]);
			details.appendChild(header);
			for (var i in lex_def.secondary_examples) {
				gfwordnet.grammar_call("?command=c-bracketedLinearize&to="+gfwordnet.langs_list.join("%20")+"&tree="+encodeURIComponent(lex_def.secondary_examples[i]),bind(extract_linearization,header),errcont);
			}
		}
	} else {
		var s   = lex_id.split("_");
		var cat = s[s.length-1];
		gfwordnet.grammar_call("?command=c-linearize&to="+gfwordnet.langs_list[index-1]+"&tree="+encodeURIComponent("MkDocument (NoDefinition \"\") (Inflection"+cat+" "+lex_id+") \"\""),bind(extract_linearization_morpho,details),errcont);
	}
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
gfwordnet.onclick_check = function (btn) {
	function errcont(text,code) { }
	function extract_confirm() {
		this.src = "checked_plus.png";
		this.onclick = function(event) { gfwordnet.onclick_minus(event, this) };
		btn.parentNode.removeChild(btn);
	}

	var img    = btn.parentNode.parentNode.firstChild.firstChild;
	var lex_id = img.nextSibling.textContent;
	gfwordnet.sense_call("?check_id="+encodeURIComponent(lex_id),bind(extract_confirm,img),errcont);
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
