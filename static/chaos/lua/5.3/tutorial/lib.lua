-- Entrance of the module.
local Knots = require("node/knots")
local Queue = require("io/queue")
Knots.Nodes:add("tutorial", {
	introduction = function()
		Queue.push_choices({ {
			t = "获取快捷按钮（推荐）",
			f = function()
			end
		}, {
			t = "开始",
			j = { "space" }
		} })
	end,
	space = function()
	end
})

table.insert(Schemes, {
	t = "教程",
	j = { "tutorial", "introduction" }
})
