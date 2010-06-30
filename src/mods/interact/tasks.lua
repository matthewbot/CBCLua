module("cbclua.interact.tasks")

local socket = require "socket"
local task = require "cbclua.task"
local connlist = require "cbclua.interact.connlist"
import "cbclua.interact.config"

local listen_socket
local broadcast_socket

function init()
	return pcall(function ()
		listen_socket = assert(socket.bind("*", INTERACT_PORT))
		broadcast_socket = assert(socket.udp())
		assert(broadcast_socket:setsockname("*", MULTICAST_PORT))
		assert(broadcast_socket:setoption("ip-add-membership", { multiaddr = MULTICAST_GROUP, interface = "*"}))
	end)
end

function listen()
	while true do
		task.sleep_io(listen_socket)
		
		connlist.new(listen_socket:accept())
	end
end

function respond()
	while true do
		task.sleep_io(broadcast_socket)
		
		local msg, ip, port = broadcast_socket:receivefrom()

		if msg == "interact" then
			local response = cbclua_get_name() .. "," .. connlist.count()
			broadcast_socket:sendto(response, ip, port)
		end
	end
end

function update_task_lists()
	while true do
		task.sleep(1)
		
		connlist.invoke_all("update_task_list")
	end
end

