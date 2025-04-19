-- 用于处理伪命令行消息队列。
module = {}

Interop = require("io/interop")

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

function module.push_choice(text)
	local p = Document:createElement("p")
	p.classList:add("line", "choice")
	p.dataset["l"] = 0

	local a = Document:createElement("a")
	a.href = "#"
	a.innerText = text

	p:appendChild(a)
	Output:appendChild(p)
end

function module.push_plain_message(html)
	module.push_raw(html, 1, { "line" })
end

return module
