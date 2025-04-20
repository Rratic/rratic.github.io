-- 定义命令（一种特殊返回值，会被调用）。
module = {}

Queue = require("io/queue")

Command = {
	type = "command",
	__call = function(mytable, ...)
		local args = { ... }
		mytable.args = args
		return mytable
	end
}

function Command:new(name, f)
	local obj = {}
	setmetatable(obj, self)
	self.__index = self
	obj.name = name
	obj.action = f
	return obj
end

function Command:run()
	self.action(self)
end

local help = Command:new("help", function(self)
	Queue.push_plain_message(self.introduction)
end)
help.introduction = "欢迎使用帮助！<br>这是一份手册列表。"
help.manual = {}

local display = Command:new("display", function(self)
	for _, v in pairs(self.args) do
		Queue.push_plain_message(tostring(v))
	end
end)

module.Command = Command
module.help = help
module.display = display

return module
