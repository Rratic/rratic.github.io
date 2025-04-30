local module = {}

GlobalTime = 0
Areas = {
	type = "areas",
	name_mapping = {},
}

function Areas:add_scheduled(name, scheme)
	self.slots[name] = {
		parent = 0,
		children = {},
		state = {
			type = "load_state",
			loaded = false,
			scheme = scheme,
		}
	}
end

return module
