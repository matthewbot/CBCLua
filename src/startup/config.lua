CBCLUA_VERSION = "CBCLua v2.0.1 Alpha"
CBCLUA_CODEPATH = os.getenv("CBCLUA_CODEPATH")
CBCLUA_MODSPATH = os.getenv("CBCLUA_MODSPATH")
CBCLUA_NAME_FILE = os.getenv("CBCLUA_NAME_FILE")

local file = io.open(CBCLUA_NAME_FILE, "r")
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

