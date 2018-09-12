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
	this.lex_ids = {};
}

gfwordnet.search = function (from, input, result) {
	function errcont(text,code) { }
	function extract_linearization(lins) {
		var indices = {"ParseBul": 1, "ParseEng": 2, "ParseSwe": 3};
		for (var i in lins) {
			var lin = lins[i];
			this[indices[lin.to]].appendChild(text(lin.text));
		}
	}
	function extract_senses(senses) {
		var index = 1;
		for (var i in senses) {
			result.appendChild(tr(node("td",{colspan: 4},[text(index+". "+senses[i].gloss)]))); index++;
			for (var lex_id in senses[i].lex_ids) {
				gfwordnet.lex_ids[lex_id] = senses[i].lex_ids[lex_id];
				
				var synonyms = senses[i].synset.slice(0);
				var index = synonyms.indexOf(lex_id);
				if (index > -1) {
					synonyms.splice(index, 1);
				}
				gfwordnet.lex_ids[lex_id].synonyms = synonyms;

				var icon;
				var row = this[lex_id];
				if (senses[i].lex_ids[lex_id].domains.indexOf("unchecked") >= 0)
					icon = img("unchecked.png");
				else
					icon = node("img", {src: "checked_plus.png", onclick: "gfwordnet.onclick_minus(event,this)"});
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
		result.appendChild(tr([th(text("Abstract")),th(text("Bulgarian")),th(text("English")),th(text("Swedish"))]));
		for (var i in lemmas) {
			var lemma = lemmas[i].lemma;
			if (!(lemma in rows)) {
				var row = [cell([text(lemma)]),cell([]),cell([]),cell([])];
				rows[lemma] = row;
				lexical_ids = lexical_ids+" "+lemma;

				gfwordnet.grammar_call("?command=c-linearize&to=ParseBul%20ParseEng%20ParseSwe&tree="+encodeURIComponent(lemma),bind(extract_linearization,row),errcont);
			}
		}
		gfwordnet.sense_call("?lexical_ids="+encodeURIComponent(lexical_ids),bind(extract_senses,rows),errcont);
    }

	gfwordnet.grammar_call("?command=c-lookupmorpho&input="+encodeURIComponent(input)+"&from="+from,extract_search,errcont);
}

gfwordnet.onclick_cell = function (cell) {
	var icon = cell.parentNode.firstChild.firstChild;
	if (icon.src.endsWith("unchecked.png"))
		return;

	function errcont(text,code) { }
	function extract_linearization(lins) {
		var langs = {"ParseBul": "Bulgarian", 
			         "ParseEng": "English", 
			         "ParseSwe": "Swedish"};
		var rows = []
		for (var i in lins) {
			var lin = lins[i];
			rows.push(tr([th(text(langs[lin.to])), td(text(lin.text))]));
		}
		this.parentNode.insertBefore(node("table",{class: "result"},rows), this.nextSibling);
	}
	function extract_linearization_synonym(lins) {
		var indices = {"ParseBul": 1, "ParseEng": 2, "ParseSwe": 3};
		for (var i in lins) {
			var lin = lins[i];
			this[indices[lin.to]].appendChild(text(lin.text));
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
		cell.parentNode.parentNode.insertBefore(tr(node("td",{colspan: 4, class: "details"},[details])), cell.parentNode.nextSibling);

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
		var row = [];
		if (this.lex_ids[lex_id].synonyms.length > 0) {
			details.appendChild(node("h1",{},[text("Synonyms")]));
			var result = node("table",{class: "result"},[]);
			result.appendChild(tr([th(text("Abstract")),th(text("Bulgarian")),th(text("English")),th(text("Swedish"))]));
			for (var i in this.lex_ids[lex_id].synonyms) {
				var row = [td([text(this.lex_ids[lex_id].synonyms[i])]),td([]),td([]),td([])]
				gfwordnet.grammar_call("?command=c-linearize&to=ParseBul%20ParseEng%20ParseSwe&tree="+encodeURIComponent(this.lex_ids[lex_id].synonyms[i]),bind(extract_linearization_synonym,row),errcont);
				result.appendChild(tr(row));
			}
			details.appendChild(result);
		}
		if (this.lex_ids[lex_id].examples.length > 0) {
			var header = node("h1",{},[text("Primary Examples")]);
			details.appendChild(header);
			for (var i in this.lex_ids[lex_id].examples) {
				gfwordnet.grammar_call("?command=c-linearize&to=ParseBul%20ParseEng%20ParseSwe&tree="+encodeURIComponent(this.lex_ids[lex_id].examples[i]),bind(extract_linearization,header),errcont);
			}
		}
		if (this.lex_ids[lex_id].secondary_examples.length > 0) {
			var header = node("h1",{},[text("Secondary Examples")]);
			details.appendChild(header);
			for (var i in this.lex_ids[lex_id].secondary_examples) {
				gfwordnet.grammar_call("?command=c-linearize&to=ParseBul%20ParseEng%20ParseSwe&tree="+encodeURIComponent(this.lex_ids[lex_id].secondary_examples[i]),bind(extract_linearization,header),errcont);
			}
		}
	} else {
		var s   = lex_id.split("_");
		var cat = s[s.length-1];
		var langs = ["ParseBul", "ParseEng", "ParseSwe"];
		gfwordnet.grammar_call("?command=c-linearize&to="+langs[index-1]+"&tree="+encodeURIComponent("MkDocument (NoDefinition \"\") (Inflection"+cat+" "+lex_id+") \"\""),bind(extract_linearization_morpho,details),errcont);
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
