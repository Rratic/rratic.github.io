-- Defines commands, tables that store arguments
-- and takes effect through `run`.
local module = {}
local Queue = require("io/queue")
local i18n = require("utils/i18n")
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
	local list = ""
	for key, _ in pairs(self.manual) do
		list = list .. key .. ", "
	end
	Queue.push_html_line(string.gsub(self.introduction, "{c}", list))
end)
help.introduction = "欢迎使用帮助！<br>可用的指令包括：{c}<br>使用 <b>/help(name)</b> 获得指令的用法信息。"
help.manual = {
	help = {
		abnf = "commands.help(name?: string)",
		description = "获取帮助信息"
	}
}

local function register(table)
	local name = table.name
	help.manual[name] = {
		abnf = table.abnf,
		description = table.description,
	}
	module[name] = Command:new(name, table.f)
end

register({
	name = "clear",
	abnf = "commands.clear()",
	description = "清除所有级别低于 10 的消息",
	f = function(self)
		Queue.clear(10, 0)
	end
})

-- todo: config

register({
	name = "describe",
	abnf = "commands.describe(table: table)",
	description = "根据 <b>title</b> 和 <b>description</b> 等字段描述 <b>table</b> 内容",
	f = function(self)
		local table = self.args[1]
		Queue.push_info(i18n.title(table) .. "\n" .. i18n.description(table))
	end
})

register({
	name = "display",
	abnf = "commands.display(...)",
	description = "以消息形式显示参数",
	f = function(self)
		-- Output with correct order.
		for i = 1, self.args.n do
			Queue.push_info(tostring(self.args[i]))
		end
	end
})

register({
	name = "preload",
	abnf = "commands.preload(code_string: string)",
	description = "注册 <b>code_string</b> 为加载完毕后在 <b>_ENV</b> 环境下运行的代码",
	f = function(self)
		if self.args.n == 1 then
			local str = self.args[1]
			LocalStorage:setItem("chaos-preload", str)
		end
	end
})

module.Command = Command
module.help = help
module.register = register

return module
