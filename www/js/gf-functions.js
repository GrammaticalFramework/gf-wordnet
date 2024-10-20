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
