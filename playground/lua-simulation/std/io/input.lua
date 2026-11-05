-- Controls command input.
require("std/interop")
local Queue = require("std/io/queue")
local Commands = require("std/io/commands")

local cli = Terminal:querySelector(".command")
local cli_id = 0
-- todo: history
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
	cli.innerText = "/"

	if command == "/" or command == "" then
		Queue.push_warning("Empty command has been skipped.")
		return
	end

	if command:sub(1, 1) == "/" then
		command = "commands." .. command:sub(2) .. ":run()"
	end

	-- Process.
	local id = "#" .. cli_id
	ProcessInput(id, command, Sandbox)
	cli_id = cli_id + 1
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
