require("io/input")
Commands = require("io/commands")
require("node/knots")

local pre = LocalStorage:getItem("chaos-preload")
ProcessInput("*pre", pre)

Nodes:run("menu", "entrance")
