function closeCheckboxes(e) {
	var table      = document.getElementById("from");
	var checkboxes = table.getElementsByTagName("TBODY")[0];
	var node = e.target;
	while (node != null) {
		if (node == checkboxes)
			break;
		node = node.parentNode;
	}

	if (node == null) {
		checkboxes.style.display = "none";
		window.removeEventListener("mousedown", closeCheckboxes);
	}	
}

function showCheckboxes(table) {
  const body = table.getElementsByTagName("TBODY")[0];
  if (body.style.display == "block") {
	body.style.display = "none";
	window.removeEventListener("mousedown", closeCheckboxes);
  } else {
    body.style.display = "block";
	window.addEventListener("mousedown", closeCheckboxes);
  }

  let tr = table.lastElementChild.firstElementChild;
  tr.firstElementChild.firstElementChild.value = "";

  tr = tr.nextElementSibling;
  while (tr != null) {
    const checkElem = tr.lastElementChild.firstElementChild;
    if (!checkElem.checked) {
        tr.style.display = "none";
    }
    tr = tr.nextElementSibling;
  }
}

function clickItem(e) {
	var item  = e.target.parentNode.previousElementSibling;
	var table = item.parentNode.parentNode.parentNode;

	// Dispatch the event.
	var multisel_event = new Event('multisel_changed');
	multisel_event.selection = getMultiSelection(table);
	multisel_event.new_current  = false;
	multisel_event.new_language = true;
	table.dispatchEvent(multisel_event);
}

function changeItem(e) {
	var item  = e.target;
	var table = item.parentNode.parentNode.parentNode;

	item.parentNode.parentNode.style.display = "none";
	table.firstElementChild.firstElementChild.firstElementChild.innerHTML = item.innerHTML;

	// Dispatch the event.
	var multisel_event = new Event('multisel_changed');
	multisel_event.selection = getMultiSelection(table);
	multisel_event.new_current  = true;
	multisel_event.new_language = false;
	table.dispatchEvent(multisel_event);

	e.stopPropagation();
}

function initMultiSelection(table) {
	selected = ["ParseBul", "ParseEng", "ParseSwe"]
	if (typeof(Storage) !== "undefined" && localStorage.lang_selection) {
		selected = JSON.parse(localStorage.lang_selection);
	}

	let tr = table.lastElementChild.firstElementChild.nextElementSibling;
	while (tr != null) {
		const nameElem  = tr.firstElementChild;
		const checkElem = tr.lastElementChild.firstElementChild;
		if (selected.includes(checkElem.name)) {
			checkElem.checked = true;
        } else {
            tr.style.display = "none";
        }
		tr = tr.nextElementSibling;
	}
}

function getMultiSelection(table) {
	var current   = table.firstElementChild.firstElementChild.firstElementChild.innerHTML;
	var selection = {current: null, langs: {}, langs_list: []};

	var tr = table.lastElementChild.firstElementChild.nextElementSibling;
	while (tr != null) {
		var nameElem  = tr.firstElementChild;
		var checkElem = tr.lastElementChild.firstElementChild;

		var name = nameElem.innerHTML;

		if (name == current)
			selection.current = checkElem.name;

		if (checkElem.checked) {
			selection.langs[checkElem.name] = {
				name:  name,
				index: selection.langs_list.length+1
			}
			selection.langs_list.push(checkElem.name);
		}
		tr = tr.nextElementSibling;
	}

	if (typeof(Storage) !== "undefined") {
		localStorage.lang_selection = JSON.stringify(selection.langs_list);
	}

	selection.isEqual = function(other) {
		if (other.langs_list.length != this.langs_list.length)
			return false;
		for (var i = 0; i < other.langs_list.length; i++) {
			if (other.langs_list[i] != this.langs_list[i])
				return false;
		}
		return true;
	}

	return selection;
}

function filterLanguages(input) {
    const filter = input.value.toLowerCase();
    const others = [];
    let match = false;
	let tr = input.parentElement.parentElement.nextElementSibling;
	while (tr != null) {
		const nameElem  = tr.firstElementChild;
		const checkElem = tr.lastElementChild.firstElementChild;
		if (!checkElem.checked) {
            if (filter != "" && nameElem.innerText.toLowerCase().startsWith(filter)) {
                match = true;
                tr.style.display = "";
            } else {
                tr.style.display = "none";
                if (filter != "") {
                    for (let alt of (nameElem.dataset.alt || "").split("|")) {
                        if (alt.toLowerCase().startsWith(filter)) {
                            others.push(tr);
                            break;
                        }
                    }
                }
            }
        }
		tr = tr.nextElementSibling;
	}
    
    if (!match) {
        for (let tr of others) {
            tr.style.display = "";
        }
    }
}
