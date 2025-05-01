local js = require "js"
Window = js.global
LocalStorage = Window.localStorage
Document = Window.document
Terminal = Document:querySelector("#terminal")
Output = Terminal:querySelector(".output")
Prompt = Terminal:querySelector(".prompt")
