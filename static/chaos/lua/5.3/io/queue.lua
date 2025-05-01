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

function module.push_html_line(html)
	module.push_raw(html, 1, { "line" })
end

function module.push_message(prepend, text, level)
	local p = Document:createElement("p")
	p.classList:add("line")
	p.classList:add("multiline")
	p.dataset["l"] = level
	p.innerHTML = prepend
	p:append(text);
	module.push(p)
end

function module.push_info(message)
	module.push_message("<span class='info'>[信息]</span> ", message, 1)
end

function module.push_success(message)
	module.push_message("<span class='success'>[成功]</span> ", message, 2)
end

function module.push_warning(message)
	module.push_message("<span class='warning'>[警告]</span> ", message, 3)
end

function module.push_error(message)
	module.push_message("<span class='error'>[错误]</span> ", message, 4)
end

function module.push_title(html)
	module.push_raw(html, 1, { "line", "title" })
end

return module
