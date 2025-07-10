local module = {}

local Chunk = require("tutorial/chunk")
local Maze = require("tutorial/maze_gen")

require("std/utils/move_table")
local Commands = require("std/io/commands")
local Queue = require("std/io/queue")

require("areas/lib")

-- todo: avoid hash collision
local function pairing(x, y)
	local a = x * 0x9e3779b1
	local b = y * 0x6a09e667
	local mixed = (a << 16) ~ (b >> 16)
	return mixed & 0xffffffff
end

local function gen(areas, ind, area)
	local x, y = table.unpack(area.position)
	Maze.gen(area.chunk, x, y)
end

local function reach_chunk(x, y)
	local key = pairing(x, y)
	if Areas.mapping[key] == nil then
		local slot = Areas:add_schedule(gen, key)
		slot.position = { x, y }
		slot.chunk = Chunk:new()
		local ind = Areas.mapping[key]
		Areas:run_schedule(ind)
	end
	return Areas.slots[Areas.mapping[key]]
end

local function reach_room_value(x, y)
	local xc, xr = x >> 5, x & 0x1f
	local yc, yr = y >> 5, y & 0x1f
	local chunk = reach_chunk(xc, yc)
	return chunk.chunk:get(xr, yr)
end

local function set_room_value(x, y, v)
	local xc, xr = x >> 5, x & 0x1f
	local yc, yr = y >> 5, y & 0x1f
	local chunk = reach_chunk(xc, yc)
	return chunk.chunk:set(xr, yr, v)
end

--[[
0 = solid wall
1 = new room
2 = empty room
3 = room with chest
4 = room with opened chest
]]

local room_descriptions = {
	"这是一个未初始化的房间。",
	"这是一个空房间。",
	"房间中央有一个箱子。",
	"房间中央有一个空箱子。",
}

local function reach_room(x, y)
	local r = reach_room_value(x, y)
	if r == 0 then
		Queue.push_line("这个方向没有门！")
		return nil
	end
	if r == 1 then
		if math.random() < 0.3 then
			r = 3
		else
			r = 2
		end
		set_room_value(x, y, r)
	end
	Queue.push_line(room_descriptions[r])
	local str = ""
	local list = {}
	for d = 1, 4 do
		local xd, yd = table.unpack(MOVE_TABLE[d])
		list[d] = reach_room_value(x + xd, y + yd) ~= 0
		if list[d] then
			str = str .. WORDS_TABLE[d] .. "，"
		end
	end
	Queue.push_line("有" .. str:gsub("，$", "") .. "方向的门。")
	return list
end

local function player_move(direction)
	Queue.clear(2, 1)
	local xd, yd = table.unpack(MOVE_TABLE[direction])
	local x, y = table.unpack(User.position)
	local result = reach_room(x + xd, y + yd)
	if type(result) ~= "nil" then
		User.position[1] = x + xd
		User.position[2] = y + yd
	end
	World.time = World.time + 10
	return result
end

local function player_give(name, number)
	if User.items[name] == nil then
		User.items[name] = number
	else
		User.items[name] = User.items[name] + number
	end
end

local function player_get()
	local x, y = table.unpack(User.position)
	local r = reach_room_value(x, y)
	if r == 3 then
		World.time = World.time + 3
		player_give("rune", 1)
		Queue.push_success("获得一块符石！")
		set_room_value(x, y, 4)
		return true
	end
	return false
end

function module.register_keys()
	Queue.push_info("已注册 w,a,s,d")
	PressCallTable["d"] = function()
		player_move(1)
	end
	PressCallTable["s"] = function()
		player_move(2)
	end
	PressCallTable["a"] = function()
		player_move(3)
	end
	PressCallTable["w"] = function()
		player_move(4)
	end
	Queue.push_info("已注册 g")
	PressCallTable["g"] = function()
		player_get()
	end
end

function module.init()
	Commands.register({
		name = "move",
		abnf = "tutorial.move(direction: integer) -> nil|table",
		description = [[
			向指定方向移动；
			1~4 分别代表东、南、西、北；
			若目标非房间，则返回 nil，否则返回长为 4 的列表，表示指定方位的门是否有效
		]],
		f = function(self)
			local direction = self.args[1]
			if direction < 1 or direction > 4 then
				return
			end
			return player_move(direction)
		end
	})

	Commands.register({
		name = "get",
		abnf = "tutorial.get() -> boolean",
		description = "拾取房间中的符石，若存在符石则必然成功，返回 true",
		f = player_get
	})

	User.position = { 1, 1 }
	User.items = {}

	Areas:init()
	reach_room(1, 1)
end

return module
