local Chunk = {}

function Chunk:new()
	local obj = {}
	setmetatable(obj, self)
	self.__index = self
	return obj
end

function Chunk:fill(integer)
	for i = 0, 1023 do
		self[i] = integer
	end
end

function Chunk:set(x, y, integer)
	self[x << 5 + y] = integer
end

function Chunk:get(x, y)
	return self[x << 5 + y]
end

return Chunk
