-- 用于输入控制。
module = {}

Interop = require("io/interop")

local cli = Terminal:querySelector(".command")
local cli_id = 0
Prompt = Terminal:querySelector(".prompt")
Prompt:addEventListener("click", function()
	-- Detect command.
	local command = cli.innerText
	if command:sub(1, 1) == "/" then
		command = "return Commands." .. command:sub(2)
	end

	-- Process.
	ProcessInput(command)

	-- Restore cli.
	cli.innerText = "/"
end)

function ProcessInput(string)
	local id = "#" .. cli_id
	local func = load(string, id, "t", _ENV)
	if type(func) == "nil" then
		Queue.push_plain_message("无法运行 " .. id)
	elseif type(func) == "function" then
		-- Run command.
		local result = func()
		if type(result) == "table" and result.type == "command" then
			result:run()
		end
	end
	cli_id = cli_id + 1
end

return module
