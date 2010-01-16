local socket = require "socket"
local task = require "cbclua.task"
local conn = require "cbclua.interact.conn"
import "cbclua.interact.config"

function listen()
	local ss = assert(socket.bind("*", CBCLUA_INTERACT_PORT))
	
	while true do
		task.sleep_io(ss)
		
		local sock = ss:accept()
		conn.InteractConnection(sock)
	end
end

function respond()
	local udp = socket.udp()
	assert(udp:setsockname("*", CBCLUA_MULTICAST_PORT))
	assert(udp:setoption("ip-add-membership", { multiaddr = CBCLUA_MULTICAST_GROUP, interface = "*"}))
	
	while true do
		task.sleep_io(udp)
		
		local msg, ip, port = udp:receivefrom()

		if msg == "interact" then
			local response = CBCLUA_NAME .. ",0"
			udp:sendto(response, ip, port)
		end
	end
end
