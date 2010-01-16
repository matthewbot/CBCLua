EvalEnvironment = create_class "EvalEnvironment"

function EvalEnvironment:construct()
	local mod = { }
	self.mod = mod
	make_cbclua_module(mod)
	
	autorequire("", mod) -- autorequire top level modules
	autorequire("cbclua.", mod) -- and cbclua modules
end

function EvalEnvironment:run(expr)
	-- compile as both a bare chunk and as an expression to be returned
	local chunk, err = loadstring(expr, "=chunk") 
	local exprchunk = loadstring("return " .. expr, "=expr")
	
	local printnil -- whether to print a nil value
	if chunk then -- if we compiled as a chunk
		printnil = false -- don't print a nil
	else
		printnil = true
	end
	
	if exprchunk then -- if we're able to compile as an expression
		chunk = exprchunk -- evaluate the expression version to get a return value
	end
	
	if chunk == nil then -- if we've got no chunk to execute
		return false, err -- fail
	end
	
	setfenv(chunk, self.mod)
	local ok, result = pcall(chunk)
	if not ok then
		return false, result
	end
	
	local resulttype = type(result)
	local resultstr
	
	if resulttype == "string" then
		resultstr = "\"" .. result .. "\""
	elseif resulttype == "nil" then
		if printnil then
			resultstr = "nil"
		else
			resultstr = "Done"
		end
	else
		resultstr = tostring(result)
	end
	
	return true, resultstr
end

