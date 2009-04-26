module(...)

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


	
