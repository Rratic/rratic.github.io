-- Manages output queue.
local module = {}
require("std/interop")

function module.line_wrap(html)
	local div = Document:createElement("div")
	div.innerHTML = html
	div.classList:add("line")
	return div
end

-- Stream.
-- Uses `dataset["l"]` to store message levels.

function module.push(element)
	OutputStream:appendChild(element)
end

function module.push_paragraph(string, type, level, classes)
	local p = Document:createElement("p")
	if type == "plain" then
		p.innerText = string
	elseif type == "markdown" then
		p.innerHTML = Marked:parse(string)
	elseif type == "html" then
		p.innerHTML = string
	end

	for i = 1, #classes do
		p.classList:add(classes[i])
	end
	p.dataset["l"] = level

	module.push(p)
end

function module.clear(level, level_decrease)
	local children = OutputStream.children
	local r = #children
	local l = 0
	while l < r do
		local child = children[l]
		local num = tonumber(child.dataset["l"])
		if num < level then
			OutputStream:removeChild(child)
			r = r - 1
		else
			child.dataset["l"] = num - level_decrease
			l = l + 1
		end
	end
end

function module.push_line(string, type)
	type = type or "markdown"
	module.push_paragraph(string, type, 1, { "line" })
end

MESSAGE_TYPES = {
	info = "信息",
	success = "成功",
	warning = "警告",
	error = "错误",
}
function module.push_message(type, text, level)
	local p = Document:createElement("p")
	p.classList:add("line")
	p.classList:add("multiline")
	p.dataset["l"] = level

	local span = Document:createElement("span")
	span.classList:add(type)
	span.innerText = "[" .. MESSAGE_TYPES[type] .. "] "
	span:addEventListener("click", function()
		OutputStream:removeChild(p)
	end)

	p:appendChild(span)
	p:append(text)
	module.push(p)
end

function module.push_info(message)
	module.push_message("info", message, 1)
end

function module.push_success(message)
	module.push_message("success", message, 2)
end

function module.push_warning(message)
	module.push_message("warning", message, 3)
end

function module.push_error(message)
	module.push_message("error", message, 4)
end

function module.push_title(html)
	module.push_paragraph(html, "plain", 1, { "line", "title" })
end

-- Fixed.

function module.push_fixed(element)
	OutputFixed:appendChild(element)
end

return module
