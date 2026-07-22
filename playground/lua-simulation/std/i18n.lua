local module = {
	current_language = "zh",
	default_language = "zh"
}

-- Reads text stored in table or string. May return nil.
function module.read(table, lang)
	lang = lang or module.current_language
	if type(table) == "string" then
		return table
	elseif type(table) == "table" then
		if type(table[lang]) == "string" then
			return table[lang]
		elseif type(table[module.default_language]) == "string" then
			return table[module.default_language]
		end
	end
end

-- Safe version of `read`, returns string only.
function module.read_safe(table, lang)
	return module.read(table, lang) or "???"
end

-- Used in mods. Adds a translation, treating different conditions of `parent[field]` type (table, string and nil).
function module.add_locale(parent, field, value, lang)
	lang = lang or module.current_language
	local original = parent[field]
	if type(original) == "table" then
		original[lang] = value
	elseif type(original) == "string" then
		parent[field] = {
			[module.default_language] = original,
			[lang] = value
		}
	elseif type(original) == "nil" then
		parent[field] = {
			[lang] = value
		}
	end
end

-- Used in mods. Modifies a translation or adds it.
function module.modify_locale(parent, field, value, lang)
	lang = lang or module.current_language
	if type(parent[field]) == "string" and module.default_language == lang then
		parent[field] = value
	else
		module.add_locale(parent, field, value, lang)
	end
end

function module.decide_locale(table, alternative)
	alternative = alternative or "???"
	if type(table) == "string" then
		return table
	elseif type(table) == "table" then
		if type(table[module.current_language]) ~= "nil" then
			return table[module.current_language]
		elseif type(table[module.default_language]) ~= "nil" then
			return table[module.default_language]
		else
			return alternative
		end
	else
		return alternative
	end
end

function module.decide_title(table)
	return module.decide_locale(table.title, table.name)
end

function module.decide_description(table)
	return module.decide_locale(table.description, table.name)
end

return module
