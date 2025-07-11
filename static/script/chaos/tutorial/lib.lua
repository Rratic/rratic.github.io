-- Entrance of the module.
local core = require("tutorial/core")
local Queue = require("std/io/queue")
require("std/nodes")

local introduction = [[
欢迎使用“教程”模组。这同时是一个使用教程和一个模组编写示例。

这里有一个无限地牢迷宫，你需要在地牢房间间移动，收集符石。

新增命令 `move` 和 `get`
]]

Nodes:add("tutorial", {
	introduction = function()
		Queue.clear(2, 1)
		Queue.push_line(introduction)
		core.register_commands()
		Nodes:jump({ "choices" })
	end,
	choices = function()
		Queue.push_choices({ {
			on = (PressCallTable["g"] == nil),
			t = "注册快捷键（推荐）",
			w = "注册 `/move` 的快捷键 `wasd` 及 `/get` 的快捷键 `g`",
			f = core.register_keys,
			j = { "choices" }
		}, {
			t = "开始",
			j = { "space" }
		} })
	end,
	space = function()
		core.init()
	end
})

table.insert(Schemes, {
	t = "教程",
	j = { "tutorial", "introduction" }
})
