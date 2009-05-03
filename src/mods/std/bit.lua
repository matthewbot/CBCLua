module(...)

local string = require "string"
local math = require "math"

-- Adapted from http://ricilake.blogspot.com/2007/10/iterating-bits-in-lua.html

function flag(p) -- first bit is bit 0
	return 2 ^ p
end 

function get(x, p) 
	return x % (p + p) >= p 
end 

function set(x, p) 
	return hasbit(x, p) and x or x + p 
end 

function clear(x, p) 
	return hasbit(x, p) and x - p or x 
end 

-- Utility functions for big-endian 16 bit words (create)

function get16(data, pos) -- reads a 16 bit value of the string data at position pos
	local high, low = string.byte(data, pos, pos+1)
	local val = high*256 + low -- no bit fiddling in lua since everything is a double internally
	if val > 32768 then
		val = val - 65536
	end
	
	return val
end

function make16(val) -- returns a new string containing the first 16 bits of val
	if val < 0 then
		val = val + 65536
	end
	
	local high = math.floor(val / 256) -- again, sans bit fiddling
	local low = val % 256
	
	return string.char(high, low)
end


	
