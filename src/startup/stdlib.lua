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

function _G.assert (v, f, ...)
  if not v then
    if f == nil then
      f = "Assertion failed!"
    end
    error (string.format (f, ...), 2)
  end
  return v
end

local math = require "math"

function math.sgn(val)
	if val == 0 then
		return 0
	elseif val > 0 then
		return 1
	else
		return -1
	end
end

function math.keepsgn(val, sgn)
	if sgn == 0 then
		error("Sign argument can't be zero!", 2)
	end
	
	val = math.abs(val)
	if sgn < 0 then
		val = -val
	end
	return val
end

	

