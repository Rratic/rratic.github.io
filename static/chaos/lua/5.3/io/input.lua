-- Controls command input.
require("io/interop")
local Queue = require("io/queue")
local Commands = require("io/commands")

local cli = Terminal:querySelector(".command")
local cli_id = 0
local Sandbox = {
	math = math,
	string = string,
	table = table,
	utf8 = utf8,
	commands = Commands,
}
Prompt:addEventListener("click", function()
	-- Detect command.
	local command = cli.innerText
	if command:sub(1, 1) == "/" then
		command = "commands." .. command:sub(2) .. ":run()"
	end

	-- Process.
	local id = "#" .. cli_id
	ProcessInput(id, command, Sandbox)
	cli_id = cli_id + 1

	-- Restore cli.
	cli.innerText = "/"
end)

function ProcessInput(id, string, env)
	local chunk, err = load(string, id, "t", env)
	if not chunk then
		Queue.push_error("Syntax Error: " .. err)
		return
	end

	local success, result = xpcall(chunk, debug.traceback)
	if not success then
		return Queue.push_error("Runtime Error: " .. result)
	end
end
