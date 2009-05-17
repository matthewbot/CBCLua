module(...)

function bind(func, arg)
	return function (...)
		return func(arg, ...)
	end
end
	
