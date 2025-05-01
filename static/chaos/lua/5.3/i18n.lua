local module = {}

Language = "zh"
DefaultLanguage = "zh"

local function local_word(name, source)
	local nDefault = Language ~= DefaultLanguage
	if type(name) == "nil" then
		name = "[?]"
	end
	if type(source) == "string" then
		if nDefault and DefaultLanguage == "en" then
			return name
		else
			return source
		end
	elseif type(source) == "table" then
		if type(source[Language]) ~= "nil" then
			return source[Language]
		elseif nDefault and type(source[DefaultLanguage]) ~= "nil" then
			return source[DefaultLanguage]
		else
			return name
		end
	else
		return name
	end
end

function module.title(table)
	return local_word(table.name, table.title)
end

function module.description(table)
	return local_word(table.name, table.description)
end

return module
