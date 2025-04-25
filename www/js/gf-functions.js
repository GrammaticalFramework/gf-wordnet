function showSearches(searchbox) {
    if (searchbox.value.length < 3) return;

	fetch("https://www.wikidata.org/w/api.php?action=wbsearchentities&language=en&uselang=en&type=item&continue=0&origin=*&format=json&search="+encodeURIComponent(searchbox.value),
          { method: "GET" })
       .then((response) => response.json())
       .then((data) => {
                const searchResults = document.getElementById("searchResults");
                searchResults.innerHTML = "";
                searchResults.style.display = "none";
                for (const res of data.search) {
                    searchResults.style.display = "block";

                    const option = document.createElement("TR");
                    option.dataset.qid = res.id;
                    if (res.label != null) {
                        const value = document.createElement("DIV");
                        value.appendChild(document.createTextNode(res.label));
                        option.appendChild(value);
                    }
                    if (res.description != null) {
                        const descr = document.createElement("DIV");
                        descr.appendChild(document.createTextNode(res.description));
                        option.appendChild(descr);
                    }

                    option.addEventListener('mouseenter', (event) => {
                        let element = searchResults.firstElementChild;
                        while (element != null) {
                            element.className = null;
                            element = element.nextElementSibling;
                        }
                        event.target.className = "current";
                    });
                    option.addEventListener('mouseleave', (event) => {
                        let element = searchResults.firstElementChild;
                        while (element != null) {
                            element.className = null;
                            element = element.nextElementSibling;
                        }
                    });
                    option.addEventListener('click', (event) => {
                        const tr = event.target.parentElement;
                        loadEntity(tr.dataset.qid, tr.firstElementChild.innerText);
                    });

                    searchResults.appendChild(option);
                    
                    if (res.label == searchbox.value) {
                        searchbox.dataset.qid = res.id;
                    }
                }
             });
}

function searchInputOnKeyPress(event) {
    if (event.key === "Enter") {
        event.preventDefault();

        let element = searchResults.firstElementChild;
        while (element != null) {
            if (element.className == "current") {
                break;
            }
            element = element.nextElementSibling;
        }
        if (element == null)
            element = searchResults.firstElementChild;
        if (element != null) {
            loadEntity(element.dataset.qid);
        }
    }
}
function searchInputOnKeyDown(event) {
    const searchResults = document.getElementById("searchResults");
    if (event.code === "ArrowUp") {
        event.preventDefault();
        let element = searchResults.firstElementChild;
        let last    = null;
        while (element != null) {
            if (element.className == "current") {
                if (last != null) {
                    element.className = null;
                    last.className = "current";
                }
                break;
            }
            last    = element;
            element = element.nextElementSibling;
        }
    } else if (event.code === "ArrowDown") {
        event.preventDefault();
        let element = searchResults.firstElementChild;
        while (element != null) {
            const next = element.nextElementSibling;
            if (element.className == "current") {
                if (next != null) {
                    element.className = null;
                    next.className = "current";
                }
                break;
            }
            element = next;
        }
        if (element == null) {
            searchResults.firstElementChild.className = "current";
        }
    }
}

function loadEntity(qid, name) {
    const searchInput = document.getElementById("searchInput");
    searchInput.value = "";
    const searchResults = document.getElementById("searchResults");
    searchResults.style.display = "none";

    const content = document.getElementById("content");
    content.dataset.qid = qid;
    content.innerText = qid+": "+name;
}

function serializeOptions(opts, choices) {
    const arr = choices ? [...choices] : [];
    for (const [c, i] of Object.entries(opts)) {
        arr.push(parseInt(c, 10), i);
    }
    return arr;
}

function deserializeOptions(arr) {
    if (arr.length % 2 !== 0) throw new Error(`Choice array must be of even length, but got ${arr.length}!`);
    const opts = {};
    for (let i = 0; i < arr.length; i += 2) {
        opts[arr[i]] = arr[i + 1];
    }
    return opts;
}

class WordNetPageState {
    constructor(elemOut, elemOpts) {
        this.elemOut = elemOut;
        this.elemOpts = elemOpts;
        this.state = { state: "initial" };
    }

    setProgram(qid, lang, code) {
        switch (this.state.state) {
            case "initial":
                this.revalidate(qid, lang, code, [], {});
                break;
            case "valid":
                if (qid === this.state.qid && lang === this.state.lang && code === this.state.code) break;
                this.revalidate(qid, lang, code, [], {});
                break;
            case "invalid":
                if (qid === this.state.qid && lang === this.state.lang && code === this.state.code) {
                    this.revalidate(qid, lang, code, this.state.choices, this.state.opts);
                } else {
                    this.revalidate(qid, lang, code, [], {});
                }
                break;
        }
    }

    setOption(choice, value) {
        switch (this.state.state) {
            case "valid":
                if (Object.hasOwn(this.state.opts, choice) && this.state.opts[choice] === value) break;
                // falls through
            case "invalid":
                const newOpts = {...this.state.opts};
                newOpts[choice] = value;
                this.revalidate(this.state.qid, this.state.lang, this.state.code, this.state.choices, newOpts);
                break;
        }
    }

    revalidate(qid, lang, code, choices, opts) {
        this.state = { state: "waiting", qid, lang, code, choices, opts };
        this.elemOut.innerHTML = "<pre class=\"status\">Evaluating...</pre>";
        this.elemOpts.innerHTML = "";
        (async () => {
            const request = { lang, code, choices: serializeOptions(opts, choices) };
            if (qid) request.qid = qid;
            const response = await fetch("FunctionsService.fcgi", {
                method: "POST",
                body: JSON.stringify(request),
            });
            if (response.status === 200) {
                this.loadResult(await response.json(), qid, lang, code, opts);
            } else {
                throw new Error(await response.text());
            }
        })().catch(error => {
            this.state = { state: "invalid", qid, lang, code, choices, opts };
            this.elemOut.innerHTML = `<pre class="status">${error}</pre>`;
        });
    }

    loadResult(result, qid, lang, code, opts) {
        const newOpts = {...opts};
        let newChoices = null;
        this.elemOut.innerHTML = "";
        this.elemOut.appendChild(node("pre",{},[text(result.msg)]));
        if (result.groups.length == 0)
            this.elemOut.appendChild(node("b",{},[text("No results")]));
        let addedOpts = false;
        for (const group of result.groups) {
            const res_tbl = node("table",{"class": "dataset"},[]);
            const row = []
            for (const header of group.headers) {
                row.push(th([text(header.label)]));
            }
            res_tbl.appendChild(tr(row))
            for (const record of group.dataset) {
                const row = []
                for (let i in record.fields) {
                    const value  = record.fields[i];
                    const header = group.headers[i];
                    if (header.type == "markup") {
                        const e = td([]);
                        e.innerHTML = value;
                        row.push(e);
                    } else if (header.type == "number") {
                        row.push(node("td",{style: "text-align: right"},[text(value)]));
                    } else if (header.type == "string") {
                        row.push(td([text(value)]));
                    } else if (header.type == "text") {
                        row.push(td(node("pre",{},[text(value)])));
                    }
                }
                res_tbl.appendChild(tr(row))

                if (!addedOpts && record.options && record.options.length) {
                    const optElems = [];
                    const validOptChoices = new Set();
                    for (const opt of record.options) {
                        validOptChoices.add(opt.choice.toString());
                        const optBody = [node("div", { "class": "option-header" }, [node("b", {}, [text(opt.label)])])];
                        const selected = opts[opt.choice] || 0;
                        newOpts[opt.choice] = selected;
                        for (let i = 0; i < opt.options.length; i++) {
                            const valueElem = node("div", { "class": "option-value" }, [text(opt.options[i])]);
                            if (i === selected) valueElem.classList.add("selected");
                            valueElem.onclick = () => this.setOption(opt.choice, i);
                            optBody.push(valueElem)
                        }
                        optElems.push(node("div", { "class": "option" }, optBody));
                    }
                    for (const c of Object.keys(opts)) {
                        if (!validOptChoices.has(c)) {
                            delete newOpts[c];
                        }
                    }
                    newChoices = record.choices;
                    this.elemOpts.appendChild(node("div", { "class": "option-container" }, optElems));
                    addedOpts = true;
                }
            }
            this.elemOut.appendChild(res_tbl);
        }
        this.state = { state: "valid", qid, lang, code, choices: newChoices, opts: newOpts };
    }
}
