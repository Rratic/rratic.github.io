local module = {}

Language = "zh"
DefaultLanguage = "zh"

-- Manages text stored in both string and table.
-- If you want to add translations in a mod, use conditioning that treats both string and table properly, so that other mods won't conflict.
function module.locale(table, lang)
	lang = lang or Language
	if type(table) == "string" then
		return table
	elseif type(table) == "table" then
		if type(table[Language]) == "string" then
			return table[Language]
		elseif type(table[DefaultLanguage]) == "string" then
			return table[DefaultLanguage]
		end
	end
	return "???"
end

local function local_word(name, source)
	local nDefault = Language ~= DefaultLanguage
	if type(name) == "nil" then
		name = "???"
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
