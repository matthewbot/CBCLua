module("main")

kissc_compat()

function main()
	fd(0)
	enable_servos()
	
	set_servo_position(0, 2)
	
	sleep(2)
end


