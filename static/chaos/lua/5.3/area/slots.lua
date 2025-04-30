function InitIdController(container)
	container.slots = {}
	container.length = 0
	container.free_indices = {}
end

function AllocId(container)
	local free = container.free_indices
	if #free == 0 then
		container.length = container.length + 1
		return container.length
	else
		local l = free[1]
		if container.slots[l + 1] == nil then
			free[1] = l + 1
		else
			table.remove(free)
		end
		return l
	end
end

function FreeId(container, id)
end

function FreeIdPeriod(container, left_bound, right_bound)
end
