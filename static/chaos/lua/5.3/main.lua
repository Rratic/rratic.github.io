Commands = require("io/commands")

Commands.display(function() return 1 end, { [0] = nil }):run()
Commands.help:run()

for i = 0, 10 do
	Queue.push_choice("Message " .. i)
end
