Project = {
	title = "函数式混沌",
	version = "0.2.0",
	platform = "web",
}

World = {
	time = 0,
}

User = {}

require("std/io/input")
require("std/nodes")
Schemes = {}

local i18n = require("std/utils/i18n")
local Queue = require("std/io/queue")

local info = [[
在输入栏键入命令，然后按提示符 `>>>` 或使用 `Ctrl + Enter` 发送。

常用命令
- `/help` 获取可用的指令列表
- `/quit` 回到主菜单
]]

Nodes:add("menu", {
	entrance = function()
		Queue.push_title(i18n.title(Project))
		Nodes:jump({ "menu" })
	end,
	menu = function()
		Queue.push_choices({ {
			t = "开始",
			j = { "beginning" }
		}, {
			t = "信息",
			w = info,
			j = { "null" }
		} })
	end,
	beginning = function()
		Queue.push_choices(Schemes)
		Nodes:jump({ "null" })
	end,
	null = function()
	end
})

local Commands = require("std/io/commands")
Commands.register({
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
if type(pre) == "string" then
	ProcessInput("preload", pre, _ENV)
end

require("tutorial/lib")
Nodes:run("menu", "entrance")
