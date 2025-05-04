-- Entrance of the module.
local core = require("tutorial/core")
local Knots = require("node/knots")
local Queue = require("io/queue")
Knots.Nodes:add("tutorial", {
	introduction = function()
		Queue.clear(2, 1)
		Queue.push_html_line("欢迎来到教程！")
		Queue.push_html_line("这是一个无限地牢迷宫，你需要在地牢房间间移动，收集符石。");
		Knots.Nodes:jump({ "choices" })
	end,
	choices = function()
		Queue.push_choices({ {
			on = (PressCallTable["g"] == nil),
			t = "注册快捷键（推荐）",
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
