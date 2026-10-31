Areas = {
	type = "areas",

	length = 0,
	current = 0,
	slots = {},

	mapping = {},
}

function Areas:allocate()
	local l = self.length + 1
	self.length = l
	return l
end

function Areas:free(ind)
	self.slots[ind] = nil
	self.length = self.length - 1
end

function Areas:add_schedule(generator, key)
	local ind = self:allocate()
	local slot = {}
	self.slots[ind] = slot
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
	if not slot.state.loaded then
		slot.state.gen(self, ind, slot)
		slot.state.loaded = true
	end
end
