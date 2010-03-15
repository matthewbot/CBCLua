module("cbclua.interact.hooks")

local connlist = require "cbclua.interact.connlist"
local cbc = require "cbclua.cbc"
local table = require "table"

local realprint = print
function _G.print(...)
	local strs = { }
	for num, val in ipairs{...} do
		strs[num] = tostring(val)
	end
	
	local str = table.concat(strs, "\t") .. "\n"
	connlist.invoke_all("send_print", str)
	
	return realprint(...)
end

local real_display_clear = cbc.display_clear
function cbc.display_clear(...)
	connlist.invoke_all("send_display_clear")
	
	return real_display_clear(...)
end

local real_beep = cbc.beep
function cbc.beep(...)
	connlist.invoke_all("send_beep")
	
	return real_beep(...)
end

