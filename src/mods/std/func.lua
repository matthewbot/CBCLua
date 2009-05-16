module(...)

-- Here be functional programming stuff!

-- List utilities

function map(func, list)
	local newlist = { }
	
	for key, val in pairs(list) do
		newlist[key] = func(val)
	end
	
	return newlist
end

function map_d(func, list) -- d for destructive updates
	for key, val in pairs(list) do
		list[key] = func(val)
	end
end

-- General purpose functional things

function ap(func, ...) -- Partial function application
	if select("#", ...) == 1 then -- don't make tables for the common single case
		local arg = ...
		return function (...)
			return func(arg, ...)
		end
	else
		local args = {...}
		return function (...)
			local moreargs = {...}
			return func(unpack(table.concat(args, moreargs)))
		end
	end
end

function bind(obj, methname)
	return ap(obj.methname, obj)
end

function liftf(func) -- takes a function, returns a new one that acts on functions
	return function (...)
		local args = { }
	
		map_d(call, args) -- call every arg
	
		return func(unpack(args))
	end
end


-- Function operators --

function call(func, ...)
	return func(...)
end

function not_(a)
	return not a
end

function gt(a, b)
	return a > b
end

function lt(a, b)
	return a < b
end

function eq(a, b)
	return a == b
end

function ne(a, b)
	return a ~= b
end

-- Predicate combinators
-- These produce functions that perform actions on other functions

not_p = liftf(not_) -- not_p takes a function and returns a new function that returns the inverse of the old
gt_p = liftf(gt) -- gt_p takes two functions and returns a new function that returns true when the first is greater than the second
lt_p = liftf(lt) -- hopefully you've figured it out by now
eq_p = liftf(eq)
ne_p = liftf(ne)
	
