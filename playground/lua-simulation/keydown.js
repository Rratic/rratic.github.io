var pressCallTable = {};
document.addEventListener("keydown", function (event) {
	let target = event.target;
	let key = event.key;
	if (!target.classList.contains("command")) {
		let f = pressCallTable[key];
		if (f instanceof Function) {
			f();
		}
	}
	else {
		if (key == "Tab" && document.hasFocus()) {
			event.preventDefault();
			insert_tab();
		}
		else if (key == "Enter" && event.ctrlKey) {
			target.previousSibling.click();
		}
	}
});

function insert_tab() {
	const selection = window.getSelection();
	if (selection.rangeCount === 0) return;
	const range = selection.getRangeAt(0);
	const spaceNode = document.createTextNode('    ');
	range.deleteContents();
	range.insertNode(spaceNode);
	range.setStartAfter(spaceNode);
	range.collapse(true);
	selection.removeAllRanges();
	selection.addRange(range);
}
