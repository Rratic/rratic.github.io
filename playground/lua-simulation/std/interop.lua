local js = require "js"
Window = js.global
LocalStorage = Window.localStorage
Document = Window.document
Terminal = Document:querySelector("#terminal")
OutputStream = Terminal:querySelector(".output.stream")
OutputFixed = Terminal:querySelector(".output.fixed")
Prompt = Terminal:querySelector(".prompt")

Marked = Window.marked
PressCallTable = Window.pressCallTable
