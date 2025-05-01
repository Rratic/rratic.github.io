-- Manages output queue.
-- Uses `dataset["l"]` to store message levels.
local module = {}
require("io/interop")

function module.push(element)
	Output:appendChild(element)
end

function module.push_raw(html, level, classes)
	-- Uses paragraph.
	local p = Document:createElement("p")
	p.innerHTML = html
	for i = 1, #classes do
		p.classList:add(classes[i])
	end
	p.dataset["l"] = level

	-- Pushes into output.
	module.push(p)
end

function module.clear(level, level_decrease)
	local children = Output.children
	local r = #children
	local l = 0
	while l < r do
		local child = children[l]
		local num = tonumber(child.dataset["l"])
		if num < level then
			Output:removeChild(child)
			r = r - 1
		else
			child.dataset["l"] = num - level_decrease
			l = l + 1
		end
	end
end

function module.push_plain_message(html)
	module.push_raw(html, 1, { "line" })
end

function module.push_info(html)
	module.push_raw(html, 1, { "line", "info" })
end

function module.push_success(html)
	module.push_raw(html, 2, { "line", "success" })
end

function module.push_warning(html)
	module.push_raw(html, 3, { "line", "warning" })
end

function module.push_error(html)
	module.push_raw(html, 4, { "line", "error" })
end

function module.push_title(html)
	module.push_raw(html, 1, { "line", "title" })
end

return module
