local module = {}

require("utils/move_table")
local Commands = require("io/commands")
local Queue = require("io/queue")

local function player_move(direction)
	local xd, yd = table.unpack(MOVE_TABLE[direction])
end

function module.register_keys()
	Queue.push_info("已注册 w,a,s,d")
	PressCallTable["d"] = function()
		player_move(1)
	end
	PressCallTable["w"] = function()
		player_move(2)
	end
	PressCallTable["a"] = function()
		player_move(3)
	end
	PressCallTable["s"] = function()
		player_move(4)
	end
	Queue.push_info("已注册 g")
	PressCallTable["g"] = function()
	end
end

function module.init()
	Commands.register({
		name = "move",
		abnf = "tutorial.move(direction: integer)",
		description = "向指定方向移动；1~4 分别代表东、北、西、南",
		f = function(self)
			local direction = self.args[1]
			if direction < 1 or direction > 4 then
				return
			end
			player_move(direction)
		end
	})

	Player.position = { 0, 0 }
	Player.items = {}

	Areas:init()
end

return module
