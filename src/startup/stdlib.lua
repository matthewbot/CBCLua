local table = require "table"

function table.map(f, tab)
	local newtab = { }
	for pos, val in ipairs(tab) do
		newtab[pos] = f(val)
	end
	return newtab
end

function table.findvalue(tab, val)
	for k, v in pairs(tab) do
		if v == val then
			return k
		end
	end
end

function table.values(tab) -- optimize me
	local key
	return function ()
		local val
		key, val = next(tab, key)
		return val
	end
end	

function table.keys(tab) -- optimize me
	local key
	return function ()
		local val
		key, val = next(tab, key)
		return key
	end
end 

function table.makeset(vals)
	local set = { }
	for _, val in pairs(vals) do
		set[val] = true
	end
	return set
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

function math.round(val)
	return math.floor(val + 0.5)
end

	

