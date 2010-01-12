module(...)

local socket = require "socket"
local task = require "cbclua.task"
import "cbclua.interact.config"

function listen()
	print("entering listen")
	local ss = assert(socket.bind("*", CBCLUA_INTERACT_PORT))
	
	while true do
		print("calling sleep_io")
		task.sleep_io(ss)
		
		local sock = ss:accept()
		
		print("Got socket!")
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
			local response = "Unnamed CBC,0"
			udp:sendto(response, ip, port)
		end
	end
end
