local module = {}

local Queue = require("io/queue")
require("node/choices")

Nodes = {
	type = "node"
}

CurrentNode = "null"

-- todo: function ProcessChoiceSet

function Nodes:add(name, knots)
	self[name] = knots
end

function Nodes:run(node, knot)
	CurrentNode = node
	self[node][knot]()
end

function Nodes:jump(dest)
	if #dest == 1 then
		self[CurrentNode][dest[1]]()
	else
		CurrentNode = dest[1]
		self[CurrentNode][dest[2]]()
	end
end

Nodes:add("menu", {
	entrance = function()
		Queue.push_title("函数式混沌")
		Nodes:run("menu", "menu")
	end,
	menu = function()
		Queue.push_choices({ {
			t = "新的开始",
			f = function()
			end,
			j = { "back" }
		}, {
			t = "信息",
			w = "使用 `/help` 获取帮助。",
			j = { "back" }
		} })
	end,
	back = function()
		Queue.push_choices({ {
			t = "返回",
			j = { "menu" }
		} })
	end,
})

module.Nodes = Nodes

return module
