-- lua class system
-- Allows instance metamethods to be overwritten

local class_mt = { }
local inst_mt = { }

function create_class(name, ...)
	local class = { }
	
	if not name:find(".", 1, true) then
		local outermod = getfenv(2)
		if outermod ~= nil then
			name = getfenv(2)._NAME .. "." .. name
		end
	end
	
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
	
	local construct = inst.construct
	if construct then
		construct(inst, ...)
	end
	
	return inst
end

function class_mt.__newindex(class, key, value)
	if type(value) == "function" then
		class.methods[key] = value
	else
		error("Trying to add a non-function type " .. type(value) .. " to a class", 2)
	end
end

function class_mt.__index(class, key)
	return class.methods[key]
end

local find_method

local function find_method_mixins(class, methname)
	for _,mixinclass in ipairs(class.mixins) do
		meth = find_method(mixinclass, methname)
		if meth then
			return meth
		end
	end
end

function find_method(class, methname)
	local meth = class.methods[methname]
	if meth then
		return meth
	end
	
	return find_method_mixins(class, methname)
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
		
		if metamethodname == "__newindex" then
			rawset(inst, ...)
		elseif metamethodname == "__tostring" then
			return "<" .. inst.class.name .. ">"
		elseif metamethodname == "__eq" then
			return rawequal(inst, ...)
		else
			error("Missing " .. metamethodname .. " metamethod for class " .. inst.class.name, 2)
		end
	end
end

local mmnames = { 'add', 'sub', 'mul', 'div', 'mod', 'pow', 'unm', 'concat', 'len', 'eq', 'lt', 'le', 'newindex', 'call', 'tostring' }

for _,mmname in pairs(mmnames) do
	mmname = "__" .. mmname
	inst_mt[mmname] = make_metamethod(mmname)
end

-- Make a super table

local findlocal

super = { }
local super_mt = { }
setmetatable(super, super_mt)

function super_mt.__index(super, methname)
	local self = findlocal(2, "self")
	if self == nil then
		error("Cannot refer to super outside of a method!", 2)
	end
	
	local meth = find_method_mixins(self.class, methname)
	if meth == nil then
		error("No superclass of class " .. self.class.name .. " contains a method named " .. methname)
	end
	
	return function (super, ...) -- return a thunk closure
		return meth(self, ...) -- that replaces the 'self' arg from the useless super table to the actual self object
	end
end
	
function findlocal(depth, name)
	local count = 1
	while true do
		local curname, curvalue = debug.getlocal(depth+1, count)
		if curname == name then
			return curvalue
		elseif not curname then
			return nil
		end
		count = count + 1
	end
end

