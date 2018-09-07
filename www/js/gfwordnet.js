gfwordnet = {}

gfwordnet.grammar_url = "http://cloud.grammaticalframework.org/robust/Parse.pgf"

gfwordnet.grammar_call=function(querystring,cont,errcont) {
    http_get_json(gfwordnet.grammar_url+querystring,cont,errcont)
}

gfwordnet.initialize = function () {
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
	function extract_search(lemmas) {
		clear(result);
		var index = 1;
		var rows  = {};
		result.appendChild(tr([th(text("Abstract")),th(text("Bulgarian")),th(text("English")),th(text("Swedish"))]));
		for (var i in lemmas) {
			var lemma = lemmas[i].lemma;
			if (!(lemma in rows)) {
				var row = [td(text(lemma)),td([]),td([]),td([])];
				rows[lemma] = row;

				result.appendChild(tr(node("td",{colspan: 4},[text(index+". gloss")]))); index++;
				result.appendChild(tr(row));

				gfwordnet.grammar_call("?command=c-linearize&to=ParseBul%20ParseEng%20ParseSwe&tree="+encodeURIComponent(lemma),bind(extract_linearization,row),errcont);
			}
		}
    }

	gfwordnet.grammar_call("?command=c-lookupmorpho&input="+encodeURIComponent(input)+"&from="+from,extract_search,errcont);
}
