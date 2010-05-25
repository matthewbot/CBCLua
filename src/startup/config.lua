local version = "CBCLua 2 v1.12"
local codepath = os.getenv("CBCLUA_CODEPATH")
local modpath = os.getenv("CBCLUA_MODSPATH")
local name_filename = os.getenv("CBCLUA_NAME_FILE")
local host = arg[1]
local name

local file = io.open(name_filename, "r")
if file then
	name=file:read()
	file:close()
else
	name="Unnamed"
end

--[[ System info functions ]]--

function cbclua_get_version()
	return version
end

function cbclua_get_name()
	return name
end

function cbclua_set_name(newname)
	name = newname
	
	local file = io.open(name_filename, "w")
	file:write(newname)
	file:close()
end

function cbclua_get_host() -- pc or chumby
	return host
end

function cbclua_get_codepath()
	return codepath
end

function cbclua_get_modpath()
	return modpath
end

