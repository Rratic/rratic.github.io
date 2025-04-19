module = {}

interop = require("io/interop")

function module.push(html, level, classes)
	-- Uses paragraph.
	local p = document:createElement("p")
	p.innerHTML = html
	for i=1, #classes do
		p.classList:add(classes[i])
	end 
	p.dataset["l"] = level

	-- Pushes into output.
	output:appendChild(p)
end

function module.plain_message(html)
	module.push(html, 1, {"line"})
end

return module
