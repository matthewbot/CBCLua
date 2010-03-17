-- Load the ext modules

require "math_ext"
require "string_ext"
require "table_ext"
require "debug_ext"
require "io_ext"

-- Patch in some (IMHO) rather stupid omissions

local set = require "set"

function set.remove(s, e)
	s[e] = nil
end

local table = require "table"

function table.findvalue(tab, val)
	for k, v in pairs(tab) do
		if v == val then
			return k
		end
	end
end

