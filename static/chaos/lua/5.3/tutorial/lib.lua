-- Entrance of the module.
local Knots = require("node/knots")
local Queue = require("io/queue")
Knots.Nodes:add("tutorial", {
	space = function()
		Queue.push_choices({ {
			t = "获取快捷按钮（推荐）"
		}, {
			t = "开始"
		} })
	end
})

table.insert(Schemes, {
	t = "教程",
	j = { "tutorial", "space" }
})
