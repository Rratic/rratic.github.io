local slots = require("area/slots")

GlobalTime = 0
Areas = {
	type = "areas",
	name_mapping = {},
}

function Areas:init()
	setmetatable(self, slots.Slots)
	self.__index = self
	self:slots_init()
end

function Areas:add_schedule(generator)
	local ind = self:alloc_ind()
	self.slots[ind].state {
		loaded = false,
		gen = generator,
	}
end

function Areas:run_schedule(ind)
	local slot = self.slots[ind]
	slot.gen(self, ind, slot)
end
