-- Entrance of the module.
local core = require("tutorial/core")
local Knots = require("node/knots")
local Queue = require("io/queue")
Knots.Nodes:add("tutorial", {
	introduction = function()
		Queue.clear(2, 1)
		Queue.push_html_line("欢迎使用这个模组！")
		Queue.push_html_line("这同时是一个使用教程和一个 <b>mod</b> 编写教程。")
		Queue.push_html_line("这里有一个无限地牢迷宫，你需要在地牢房间间移动，收集符石。");
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
		Queue.push_html_line("新增了命令 <b>move</b> 和 <b>get</b>。<br>使用 <b>/help(\"move\")</b> 获取用法。")
		core.init()
	end
})

table.insert(Schemes, {
	t = "教程",
	j = { "tutorial", "introduction" }
})
