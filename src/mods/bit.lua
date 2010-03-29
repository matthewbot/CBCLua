--- Pure-lua bit manipulation functions
module("cbclua.bit")

local string = require "string"
local math = require "math"

-- Adapted from http://ricilake.blogspot.com/2007/10/iterating-bits-in-lua.html

--- Returns a single bit flag with the p'th bit set
-- @param p The bit to be set, starting at 0 for the least significant bit
-- @return A bitfield with a single bit set
function flag(p) -- first bit is bit 0
	return 2 ^ p
end 

--- Returns true if the p'th bit of x is set, false otherwise
-- @param x The bit field to check
-- @param p The number of the bit to check
-- @return true if bit is set, false if not
-- @see set
-- @see clear
function get(x, p)
	return get_flag(x, flag(p))
end

--- Returns true if the specified flag is set in x, false otherwise
-- @param x The bit field to check
-- @param f The flag to check
-- @return true if the flag is set, false if not
-- @see flag
-- @see set_flag
-- @see clear_flag
function get_flag(x, f) 
	return x % (f + f) >= f
end 

--- Returns x with its p'th bit set
-- @param x The bit field to start with
-- @param p The bit to set
-- @return The new bit field
-- @see get
-- @see clear
function set(x, p)
	return set_flag(x, flag(p))
end

--- Returns x with the f flag set
-- @param x The bit field to start with
-- @param f The flag to set
-- @return The new bit field
-- @see flag
-- @see get_flag
-- @see clear_flag
function set_flag(x, f)
	return get_flag(x, f) and x or x + f
end 

--- Returns x with its p'th bit unset
-- @param x the bit field to start with
-- @param p the bit to unset
-- @return the new bit field
-- @see get
-- @see set
function clear(x, p) 
	return clear_flag(x, flag(p))
end

--- Returns x with the f flag unset
-- @param x the bit field to start with
-- @param f the flag to unset
-- @return the new bit field
-- @see flag
-- @see get_flag
-- @see set_flag
function clear_flag(x, f)
	return get_flag(x, f) and x - f or x 
end 

-- Utility functions for big-endian 16 bit words (create)

--- Interprets pos and pos+1th byte of data as a 16-bit big endian signed integer
-- @param data The string to pull bytes from
-- @param pos The position in the string to read
-- @return a signed 16-bit integer
-- @see put16
-- @see getu16
function get16(data, pos) -- reads a 16 bit value of the string data at position pos
	local val = getu16(data, pos)
	
	if val > 32768 then
		val = val - 65536
	end
	
	return val
end

--- Returns a two byte string containing val as a 16-bit big endian signed integer
-- @param val A number that can be represented as a 16-bit signed integer (-16384 to 16383)
-- @see get16
-- @see makeu16
function make16(val) -- returns a new string containing the first 16 bits of val
	if val < 0 then
		val = val + 65536
	end
	
	return makeu16(val)
end

--- Interprets pos and pos+1th byte of data as a 16-bit big endian unsigned integer
-- @param data The string to pull bytes from
-- @param pos The position in the string to read
-- @see putu16
-- @see get16
function getu16(data, pos) -- reads a 16 bit value of the string data at position pos
	local high, low = string.byte(data, pos, pos+1)
	local val = high*256 + low -- no bit fiddling in lua since everything is a double internally
	
	return val
end

--- Returns a two byte string containing val as a 16-bit big endian unsigned integer
-- @param val A number that can be represented as a 16-bit unsigned integer (0 to 32768)
-- @see getu16
-- @see make16
function makeu16(val) -- returns a new string containing the first 16 bits of val
	local high = math.floor(val / 256) -- again, sans bit fiddling
	local low = val % 256
	
	return string.char(high, low)
end
	
