module("std.class")

local class_mt = { }
local inst_mt = { }

function create(name, ...)
	local class = { }
	class.name = name
	class.mixins = {...}
	class.methods = { }
	setmetatable(class, class_mt)
	
	return class
end

function class_mt.__call(class, ...)
	local inst = { }
	inst.class = class
	setmetatable(inst, inst_mt)
	
	local constr = inst.construct
	if constr then
		constr(inst, ...)
	end
	
	return inst
end

function class_mt.__newindex(class, key, value)
	if type(value) == "function" then
		class.methods[key] = value
	else
		error("Trying to add something other than a function to a class", 2)
	end
end

function class_mt.__index(class, key)
	return class.methods[key]
end

local function find_method(class, methname)
	local meth = class.methods[methname]
	if meth then
		return meth
	end
	
	for _,mixinclass in ipairs(class.mixins) do
		meth = find_method(mixinclass, methname)
		if meth then
			return meth
		end
	end
end

function inst_mt.__index(inst, key)
	local meth = find_method(inst.class, key)
	if meth then 
		return meth
	end
	
	local metameth = find_method(inst.class, '__index')
	if metameth then
		return metameth(inst, key)
	end
end

-- Hold on to your hats, its metaprogramming time!

local function make_metamethod(metamethodname)
	return function (inst, ...)
		local metameth = find_method(inst.class, metamethodname)
		
		if metameth then
			return metameth(inst, ...)
		end
		
		error("Missing " .. metamethod .. " metamethod for class " .. inst.class.name, 2)
	end
end

local mmnames = { 'add', 'sub', 'mul', 'div', 'mod', 'pow', 'unm', 'concat', 'len', 'eq', 'lt', 'le', 'newindex', 'call' }

for _,mmname in pairs(mmnames) do
	inst_mt[mmname] = make_metamethod(mmname)
end
