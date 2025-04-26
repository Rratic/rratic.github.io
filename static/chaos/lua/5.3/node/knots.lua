local module = {}

local Queue = require("io/queue")
require("node/choices")

Nodes = {
	type = "node"
}

-- todo: function ProcessChoiceSet

function Nodes:add(name, knots)
	self[name] = knots
end

function Nodes:run(node, knot)
	self[node][knot]()
end

Nodes:add("menu", {
	entrance = function()
		Queue.push_title("函数式混沌")
		Queue.push_choices({ {
			t = "新的开始",
			f = function()
			end
		}, {
			t = "信息",
			f = function()
			end
		} })
	end
})

module.Nodes = Nodes

return module
