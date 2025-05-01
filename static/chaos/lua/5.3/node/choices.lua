local Queue = require("io/queue")
local Knots = require("node/knots")

function Queue.push_choices(list)
	local p = Document:createElement("p")
	p.classList:add("line")
	p.dataset["l"] = 0

	for i = 1, #list do
		local item = list[i]

		local pp = Document:createElement("p")
		pp.classList:add("choice")

		local a = Document:createElement("a")
		a.href = "#"
		a.innerText = item.t
		a:addEventListener("click", function()
			Queue.clear(1, 0)
			if item.w ~= nil then
				Queue.push_html_line(item.w)
			end
			if item.f then
				item.f()
			end
			if item.j ~= nil then
				Knots.Nodes:jump(item.j)
			end
		end)

		pp:appendChild(a)
		p:appendChild(pp)
	end

	Output:appendChild(p)
end
