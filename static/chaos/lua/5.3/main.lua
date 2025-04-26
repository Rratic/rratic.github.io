require("io/input")
Commands = require("io/commands")
require("node/choices")

local pre = LocalStorage:getItem("chaos-preload")
ProcessInput("*pre", pre)
