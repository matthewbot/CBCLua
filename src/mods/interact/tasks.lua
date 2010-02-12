local socket = require "socket"
local task = require "cbclua.task"
local connlist = require "cbclua.interact.connlist"
import "cbclua.interact.config"

function listen()
	local ss = assert(socket.bind("*", INTERACT_PORT))
	
	while true do
		task.sleep_io(ss)
		
		connlist.new(ss:accept())
	end
end

function respond()
	local udp = socket.udp()
	assert(udp:setsockname("*", MULTICAST_PORT))
	assert(udp:setoption("ip-add-membership", { multiaddr = MULTICAST_GROUP, interface = "*"}))
	
	while true do
		task.sleep_io(udp)
		
		local msg, ip, port = udp:receivefrom()

		if msg == "interact" then
			local response = cbclua_get_name() .. "," .. connlist.count()
			udp:sendto(response, ip, port)
		end
	end
end

function update_task_lists()
	while true do
		task.sleep(1)
		
		connlist.invoke_all("update_task_list")
	end
end

