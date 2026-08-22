local module = {
	storage = {},
}

local Commands = require("std/io/commands")
Commands.register({
	name = "config",
	abnf = "commands.config(key: string, value: any)",
	description = "设置配置项的值",
	f = function(self)
		module.storage[self.args[1]] = self.args[2]
	end
})

return module
