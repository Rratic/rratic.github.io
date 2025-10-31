Configs = {}

local function insert(key, info)
	Configs[key] = info
	if info.read then
		info:read()
	end
end

local Commands = require("std/io/commands")
Commands.register({
	name = "config",
	abnf = "commands.config(key: string, value: any)",
	description = "设置配置项的值",
	f = function(self)
		local key = self.args[1]
		Configs[key].data = self.args[2]
		if Configs[key].write then
			Configs[key]:write()
		end
	end
})

return { insert = insert }
