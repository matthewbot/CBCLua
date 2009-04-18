-- some handy global functions

function table.findvalue(table, val)
	for k,v in pairs(table) do
		if v == val then
			return k
		end
	end
end
