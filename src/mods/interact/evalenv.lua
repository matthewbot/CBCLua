EvalEnvironment = create_class "EvalEnvironment"

function EvalEnvironment:construct()
	local mod = { }
	self.mod = mod
	make_cbclua_module(mod)
	
	autorequire("", mod) -- autorequire top level modules
	autorequire("cbclua.", mod) -- and cbclua modules
end

function EvalEnvironment:run(expr)
	local printnil = true
	local chunk, err = loadstring("return " .. expr, "=expr")
	
	if chunk == nil then
		chunk, err = loadstring(expr, "=chunk")
		printnil = false
		if chunk == nil then
			return false, err
		end
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

