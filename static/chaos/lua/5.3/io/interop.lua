module = {}

local js = require "js"
local window = js.global
document = window.document
terminal = document:getElementById("terminal")
output = terminal:querySelector(".output")

return module
