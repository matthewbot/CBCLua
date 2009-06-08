module(...)

local task = require "std.task"
local util = require "std.util"
local raw = require "raw.cbc"
local math = require "math"
local os = require "os"

global{
	image_width = 160,
	image_height = 120
}

-- Public Functions

function update()
	task.wait(has_new_frame)
	raw.track_update()
end

function has_new_frame()
	return raw.track_is_new_data_available() == 1 -- TODO fix in C library
end

function run(func)
	local result

	repeat
		task.yield()
		update()
		result = func()
	until result ~= nil
	
	return result
end

-- Channel and Blob

local Channel = create_class "Channel"
local Blob = create_class "Blob"

function Channel:construct(num)
	self.num = num
	self.blobs = { }
end

function Channel:get_blob(num)
	if num == nil then
		num = 0
	end

	local blob = self.blobs[num]
	if blob then
		return blob
	end
	
	blob = Blob(num, self)
	self.blobs[num] = blob
	return blob
end

Channel.__index = Channel.get_blob

function Channel:get_count()
	return raw.track_count(self.num)
end

function Channel:get_all()
	local blobs = { }

	for i=1,self:get_count() do
		blobs[i] = self:get_blob(i)
	end
	
	return blobs
end

function Channel:find(pred)
	for i=1,self:get_count() do
		local blob = self:get_blob()
		
		if pred(blob) then
			return blob
		end
	end
end

function Blob:construct(num, chan)
	self.num = num
	self.chan = chan
end

function Blob:__index(key)
	key = "track_" .. key
	
	local func = Blob[key] -- look up as a function of Blob
	if func then
		return func(self)
	end

	func = raw[key]
	if func then
		return func(self.chan.num, self.num)
	end
end

local xscale = (image_width - 1)/2
function Blob:track_x_float()
	return (self.x - xscale) / xscale
end

local yscale = (image_width - 1)/2
function Blob:track_y_float()
	return (yscale - self.y) / yscale
end

function Blob:horiz_dist_to(other)
	local dist
	if self.x > other.x then
		dist = self.bbox_left - other.bbox_right
	else
		dist = other.bbox_left - self.bbox_right
	end
	
	if dist < 0 then
		return 0
	else
		return dist
	end
end

function Blob:vert_dist_to(other)
	local dist
	if self.y > other.y then
		dist = self.bbox_top - other.bbox_bottom
	else
		dist = other.bbox_top - self.bbox_bottom
	end
	
	if dist < 0 then
		return 0
	else
		return dist
	end
end

global("channels", { })

for i=0,3 do 
	channels[i] = Channel(i)
end

-- CBC performance tweak

local util = require "std.util"

util.set_cbc_proc("/sys/class/video4linux/video0/auto_exposure", 0)
util.set_cbc_proc("/sys/class/video4linux/video0/exposure", 1)
