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

function serializeInput(opts) {
    const arr = [];
    for (const oi of opts) {
        arr.push(oi.choice, oi.value);
    }
    return arr;
}

class WNStateChangeEvent extends Event {
    constructor(newState) {
        super("state");
        this.newState = newState;
    }
}

class WNResultEvent extends Event {
    constructor(result) {
        super("result");
        this.result = result;
    }
}

class WNClient extends EventTarget {
    constructor() {
        super();
        this.state = { state: "initial" };
    }

    setState(state) {
        this.state = state;
        this.dispatchEvent(new WNStateChangeEvent(state));
    }

    setProgram(qid, lang, code) {
        switch (this.state.state) {
            case "initial":
                this.revalidate(qid, lang, code, []);
                break;
            case "valid":
            case "interactive":
                if (qid === this.state.qid && lang === this.state.lang && code === this.state.code) break;
                this.revalidate(qid, lang, code, []);
                break;
            case "invalid":
                if (qid === this.state.qid && lang === this.state.lang && code === this.state.code) {
                    this.revalidate(qid, lang, code, serializeInput(this.state.opts));
                } else {
                    this.revalidate(qid, lang, code, []);
                }
                break;
            default:
                throw new Error(`Bad state: ${this.state.state}`);
        }
    }

    setInteractionPoint(headers, record) {
        switch (this.state.state) {
            case "valid":
            case "interactive":
                this.setState({
                    state: "interactive", headers, record,
                    qid: this.state.qid, lang: this.state.lang, code: this.state.code,
                    opts: record.options
                });
                break;
            default:
                throw new Error(`Bad state: ${this.state.state}`);
        }
    }

    setOption(choice, value) {
        let index = -1;
        for (let i in this.state.opts) {
            if (this.state.opts[i].choice == choice) {
                index = i;
                break;
            }
        }
        if (index < 0)
            return;

        switch (this.state.state) {
            case "interactive":
                if (this.state.opts[index].value == value)
                    break;
                // falls through
            case "invalid":
                const input = serializeInput(this.state.opts);
                input[2*index+1] = value;
                this.revalidate(this.state.qid, this.state.lang, this.state.code, input);
                break;
            default:
                throw new Error(`Bad state: ${this.state.state}`);
        }
    }

    revalidate(qid, lang, code, input) {
        this.setState({ state: "waiting", qid, lang, code, opts: [] });
        (async () => {
            const request = { lang, code, input: input };
            if (qid) request.qid = qid;
            const response = await fetch("FunctionsService.fcgi", {
                method: "POST",
                body: JSON.stringify(request),
            });
            if (response.status === 200) {
                this.loadResult(await response.json(), qid, lang, code);
            } else {
                throw new Error(await response.text());
            }
        })().catch(error => {
            this.setState({ state: "invalid", error, qid, lang, code, opts: [] });
        });
    }

    loadResult(result, qid, lang, code) {
        this.dispatchEvent(new WNResultEvent(result));
        this.setState({ state: "valid", groups: result.groups, qid, lang, code });
    }
}
