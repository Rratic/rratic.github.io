local module = {}
local Queue = require("io/queue")

function Queue.push_choices(list)
	local p = Document:createElement("p")
	p.classList:add("line")
	p.dataset["l"] = 0

	for i = 1, #list do
		local pp = Document:createElement("p")
		pp.classList:add("choice")

		local a = Document:createElement("a")
		a.href = "#"
		a.innerText = list[i]

		pp:appendChild(a)
		p:appendChild(pp)
	end

	Output:appendChild(p)
end

return module
