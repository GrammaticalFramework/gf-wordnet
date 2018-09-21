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
			           "ParsePor": {name: "Portuguese", index: 3},
			           "ParseSwe": {name: "Swedish",    index: 4}};
	this.langs_list = ["ParseBul", "ParseEng", "ParsePor", "ParseSwe"];
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
			result.appendChild(tr(node("td",{colspan: 1 + gfwordnet.langs_list.length + (gfwordnet.can_check ? 1 : 0)},[text(index+". "+senses[i].gloss)]))); index++;
			for (var lex_id in senses[i].lex_ids) {
				gfwordnet.lex_ids[lex_id] = senses[i].lex_ids[lex_id];
				
				var synonyms = senses[i].synset.slice(0);
				var k = synonyms.indexOf(lex_id);
				if (k > -1) {
					synonyms.splice(k, 1);
				}
				gfwordnet.lex_ids[lex_id].synonyms = synonyms;

				var icon;
				var row = this[lex_id];
				var domains = senses[i].lex_ids[lex_id].domains;
				if (domains.indexOf("unchecked") >= 0 && domains.indexOf("checked") < 0) {
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
		if (gfwordnet.can_check)
			row.push(node("th",{style: "width: 10px"},[]));
		result.appendChild(tr(row));

		for (var i in lemmas) {
			var lemma = lemmas[i].lemma;
			if (!(lemma in rows)) {
				var row = [cell([text(lemma)])];
				for (var lang in gfwordnet.langs) {
					row.push(cell([]));
				}
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

gfwordnet.onclick_cell = function (cell) {
	var icon = cell.parentNode.firstChild.firstChild;
	if (icon.src.endsWith("unchecked.png"))
		return;

	function errcont(text,code) { }
	function extract_linearization(lins) {
		var rows = []
		for (var i in lins) {
			var lin = lins[i];
			rows.push(tr([th(text(gfwordnet.langs[lin.to].name)), td(text(lin.text))]));
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
		cell.parentNode.parentNode.insertBefore(tr(node("td",{colspan: 1 + gfwordnet.langs_list.length + (gfwordnet.can_check ? 1 : 0), class: "details"},[details])), cell.parentNode.nextSibling);

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
				gfwordnet.grammar_call("?command=c-linearize&to="+gfwordnet.langs_list.join("%20")+"&tree="+encodeURIComponent(this.lex_ids[lex_id].synonyms[i]),bind(extract_linearization_synonym,row),errcont);
				result.appendChild(tr(row));
			}
			details.appendChild(result);
		}
		if (this.lex_ids[lex_id].examples.length > 0) {
			var header = node("h1",{},[text("Examples")]);
			details.appendChild(header);
			for (var i in this.lex_ids[lex_id].examples) {
				gfwordnet.grammar_call("?command=c-linearize&to="+gfwordnet.langs_list.join("%20")+"&tree="+encodeURIComponent(this.lex_ids[lex_id].examples[i]),bind(extract_linearization,header),errcont);
			}
		}
		if (this.lex_ids[lex_id].secondary_examples.length > 0) {
			var header = node("h1",{},[text("Secondary Examples")]);
			details.appendChild(header);
			for (var i in this.lex_ids[lex_id].secondary_examples) {
				gfwordnet.grammar_call("?command=c-linearize&to="+gfwordnet.langs_list.join("%20")+"&tree="+encodeURIComponent(this.lex_ids[lex_id].secondary_examples[i]),bind(extract_linearization,header),errcont);
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
