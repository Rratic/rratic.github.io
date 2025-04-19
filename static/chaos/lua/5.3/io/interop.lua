module = {}

local js = require "js"
Window = js.global
Document = Window.document
Terminal = Document:querySelector("#terminal")
Output = Terminal:querySelector(".output")

return module
