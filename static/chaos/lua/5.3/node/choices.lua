local Queue = require("io/queue")

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
			Queue.clear(0.5, 0)
			if item.w ~= nil then
				Queue.push_plain_message(item.w)
			end
			if item.f then
				item.f()
			end
			if item.j ~= nil then
				Nodes:jump(item.j)
			end
		end)

		pp:appendChild(a)
		p:appendChild(pp)
	end

	Output:appendChild(p)
end
