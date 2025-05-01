local module = {}

local Nodes = {
	type = "nodes",
	current = "null",
	map = {},
}

-- todo: function ProcessChoiceSet

function Nodes:add(name, knots)
	self.map[name] = knots
end

function Nodes:run(node, knot)
	self.current = node
	self.map[node][knot]()
end

function Nodes:jump(dest)
	if #dest == 1 then
		self.map[self.current][dest[1]]()
	else
		self.current = dest[1]
		self.map[self.current][dest[2]]()
	end
end

module.Nodes = Nodes

return module
