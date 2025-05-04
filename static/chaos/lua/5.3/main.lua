Game = {
	title = "函数式混沌",
	version = "0.1.0",
	platform = "web",
}

World = {
	time = 0,
}

Player = {}

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
			w = "在输入栏键入命令，然后按提示符 <b>&gt;&gt;&gt;</b> 或使用 <b>Ctrl + Enter</b> 发送。<br>使用命令 <b>/help</b> 获取帮助。",
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
			cl = true,
			j = { "menu" }
		} })
	end,
})

local Commands = require("io/commands")
Commands.quit = Commands.register({
	name = "quit",
	abnf = "commands.quit()",
	description = "回到主菜单",
	f = function()
		Queue.clear(100, 1)
		Nodes:run("menu", "entrance")
	end
})

PressCallTable["Escape"] = function()
	Commands.quit:run()
end

local pre = LocalStorage:getItem("chaos-preload")
ProcessInput("*pre", pre, _ENV)

require("tutorial/lib")
Nodes:run("menu", "entrance")
