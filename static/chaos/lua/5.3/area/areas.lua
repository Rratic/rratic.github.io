local Slots = require("area/slots")

Areas = {
	type = "areas",
	mapping = {},
}

function Areas:init()
	self.slots_init = Slots.slots_init
	self.alloc_ind = Slots.alloc_ind
	self.free_ind = Slots.free_ind
	self:slots_init()
end

function Areas:add_schedule(generator, key)
	local ind = self:alloc_ind()
	local slot = self.slots[ind]
	if key then
		self.mapping[key] = ind
	end
	slot.state = {
		loaded = false,
		gen = generator,
	}
	return slot
end

function Areas:run_schedule(ind)
	local slot = self.slots[ind]
	slot.gen(self, ind, slot)
end
