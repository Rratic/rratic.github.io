Game = {
	title = "函数式混沌",
	version = "0.1.0",
	platform = "web",
}

require("io/input")
require("node/choices")
require("area/areas")
Schemes = {}

local i18n = require("utils/i18n")
local Queue = require("io/queue")
local Knots = require("node/knots")
local Nodes = Knots.Nodes
Nodes:add("menu", {
	entrance = function()
		Queue.push_title(i18n.title(Game))
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
		Queue.push_html_line("可选的方案：")
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
ProcessInput("*pre", pre, _ENV)

require("tutorial/lib")
Nodes:run("menu", "entrance")
