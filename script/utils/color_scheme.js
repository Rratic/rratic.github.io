var savedTheme = localStorage.getItem("linkita-color-scheme");
var browserDark = window.matchMedia("(prefers-color-scheme: dark)").matches;
if (savedTheme === "dark" || (savedTheme == undefined && browserDark)) {
	document.body.classList.add("dark");
	localStorage.setItem("linkita-color-scheme", "dark");
}
else {
	localStorage.setItem("linkita-color-scheme", "light");
}
