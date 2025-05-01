-- Manages slots.
local module = {}
local Slots = {}

function Slots:slots_init()
	self.slots = {}
	self.length = 0
	-- self.free_indices = {}
end

function Slots:alloc_ind()
	--[[
	local free = self.free_indices
	local free_num = #free
	if free_num == 0 then
		self.length = self.length + 1
		return self.length
	else
		local l = free[1]
		if self.slots[l + 1] == nil then
			free[1] = l + 1
		else
			table.remove(free)
		end
		return l
	end
	]]
	local l = self.length + 1
	self.length = l
	self.slots[l] = {
		parent = 0,
		-- children = {},
	}
	return l
end

function Slots:free_ind(ind)
	self.slots[ind] = nil
	self.length = self.length - 1
end

module.Slots = Slots

return module
