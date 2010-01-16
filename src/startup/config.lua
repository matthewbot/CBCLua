CBCLUA_VERSION = "CBCLua v2.0.1 Alpha"

local CBCLUA_NAME_FILENAME = os.getenv("CBCLUA_NAME")
local file = io.open(CBCLUA_NAME_FILENAME, "r")
if file then
	CBCLUA_NAME=file:read()
	file:close()
else
	CBCLUA_NAME="Unnamed"
end

function change_cbclua_name(newname)
	CBCLUA_NAME = newname
	local file = io.open(CBCLUA_NAME_FILENAME, "w")
	file:write(newname)
	file:close()
end

