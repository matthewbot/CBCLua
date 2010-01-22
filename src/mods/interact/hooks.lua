local connlist = require "cbclua.interact.connlist"
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

