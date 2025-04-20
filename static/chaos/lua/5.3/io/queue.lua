-- 用于处理伪命令行消息队列。
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

function module.push_plain_message(html)
	module.push_raw(html, 1, { "line" })
end

function module.push_title(html)
	module.push_raw(html, 1, { "line", "title" })
end

return module
