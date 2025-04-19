queue = require("io/queue")

for i=0, 10 do
	queue.plain_message("Message " .. i)
end
