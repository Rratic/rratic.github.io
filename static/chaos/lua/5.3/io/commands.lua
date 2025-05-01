-- Defines commands, tables that store arguments
-- and takes effect through `run`.
local module = {}
local Queue = require("io/queue")
local i18n = require("i18n")
require("io/interop")

local Command = {
	type = "command",
	args = {},
	__call = function(mytable, ...)
		-- Does not jump over `nil`.
		local args = table.pack(...)
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
	self.args = {}
end

local function explain_function(table)
	local desc = i18n.description(table)
	Queue.push_html_line("<b>" .. table.abnf .. "</b> " .. desc)
end

local help = Command:new("help", function(self)
	if self.args.n == 1 then
		explain_function(self.manual[self.args[1]])
		return
	end
	Queue.push_html_line(self.introduction)
	for _, value in pairs(self.manual) do
		explain_function(value)
	end
end)
help.introduction = "欢迎使用帮助！<br>这是一份手册列表。"
help.manual = {}

local function record_manual(name, docstring)
	help.manual[name] = docstring
end

record_manual("help", {
	abnf = "commands.help([name])",
	description = "获取帮助信息"
})

local clear = Command:new("clear", function(self)
	Queue.clear(10, 0)
end)
record_manual("clear", {
	abnf = "commands.clear()",
	description = "清除所有级别低于 10 的消息",
})

local display = Command:new("display", function(self)
	-- Output with correct order.
	for i = 1, self.args.n do
		Queue.push_info(tostring(self.args[i]))
	end
end)
record_manual("display", {
	abnf = "commands.display(...)",
	description = "以消息形式显示参数",
})

local preload = Command:new("preload", function(self)
	if self.args.n == 1 then
		local str = self.args[1]
		LocalStorage:setItem("chaos-preload", str)
	end
end)
record_manual("preload", {
	abnf = "commands.preload(code_string)",
	description = "注册预加载代码",
})

module.Command = Command
module.help = help
module.clear = clear
module.display = display
module.preload = preload

return module
