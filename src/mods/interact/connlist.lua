local connmod = require "cbclua.interact.conn"
local table = require "table"

local connset = { }
local conncount = 0

local callbacks = { }

function new(sock)
	local conn = connmod.InteractConnection(sock, callbacks)
	connset[conn] = true
	conncount = conncount + 1
	return conn
end

function remove(conn)
	if connset[conn] then
		connset[conn] = nil
		conncount = conncount - 1
	end
end

function count()
	return conncount
end

function invoke_all(funcname, ...)
	for conn in pairs(connset) do
		conn[funcname](conn, ...)
	end
end

function callbacks.on_conn_close(conn)
	return remove(conn)
end

