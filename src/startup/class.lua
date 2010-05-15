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
	class.supers = {...}
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

local find_method

function class_mt.__index(class, key)
	return find_method(class, key)
end

function find_method(class, methname)
	local meth = class.methods[methname]
	if meth then
		return meth
	end
	
	for _,super in ipairs(class.supers) do
		meth = find_method(super, methname)
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

function is_a(obj, class)
	return is_subclass(class, obj.class)
end

function is_subclass(class, super)
	-- see if we're the same class
	if class == super then 
		return true
	end
		
	-- otherwise, see if any of our superclasses are subclasses of super
	for _, class_super in ipairs(class.supers) do
		if is_baseclass(class_super, super) then
			return true
		end
	end
	
	return false
end
	
