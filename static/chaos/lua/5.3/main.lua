require("io/input")
require("node/choices")
require("area/areas")
Commands = require("io/commands")
Schemes = {}

local Queue = require("io/queue")
local Knots = require("node/knots")
local Nodes = Knots.Nodes
Nodes:add("menu", {
	entrance = function()
		Queue.push_title("函数式混沌")
		Nodes:jump({ "menu" })
	end,
	menu = function()
		Queue.push_choices({ {
			t = "新的开始",
			j = { "beginning" }
		}, {
			t = "信息",
			w = "使用 <b>/help</b> 获取帮助。",
			j = { "back" }
		} })
	end,
	beginning = function()
		Queue.push_plain_message("可选的方案：")
		Queue.push_choices(Schemes)
		Nodes:jump({ "back" })
	end,
	back = function()
		Queue.push_choices({ {
			t = "返回",
			f = function()
				Queue.clear(2, 1)
			end,
			j = { "menu" }
		} })
	end,
})

local pre = LocalStorage:getItem("chaos-preload")
ProcessInput("*pre", pre)

Nodes:run("menu", "entrance")
