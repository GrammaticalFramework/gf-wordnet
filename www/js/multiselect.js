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
  var checkboxes = table.getElementsByTagName("TBODY")[0];
  if (checkboxes.style.display == "block") {
	checkboxes.style.display = "none";
	window.removeEventListener("mousedown", closeCheckboxes);
  } else {
	checkboxes.style.display = "block";
	window.addEventListener("mousedown", closeCheckboxes);
  }
}

function changeItem(e,item) {
	item.parentNode.parentNode.style.display = "none";
	item.parentNode.parentNode.parentNode.firstElementChild.firstElementChild.firstElementChild.innerHTML = item.innerHTML;
	e.stopPropagation();				
}

function getMultiSelection(table) {
	var current   = table.firstElementChild.firstElementChild.firstElementChild.innerHTML;
	var selection = {current: null, langs: {}, langs_list: []};

	var tr = table.lastElementChild.firstElementChild;
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
