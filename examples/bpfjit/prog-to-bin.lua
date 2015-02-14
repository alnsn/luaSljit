#!/usr/bin/env lua5.3
-- Save an output of tcpdump -dd as a binary file.

local args = {...}

-- Convert one line of tcpdump -dd, e.g. "{ 0x15, 3, 4, 0x00000006 },"
-- to four return values 0x15, 3, 4, 0x06.
-- WARNING this function evaluates the input string!
local function load_insn(line)
	local chunk = assert(load("return " .. line .. "nil"))
	local insn = chunk() -- eval
	return insn[1], insn[2], insn[3], insn[4]
end

local out = assert(io.open(args[1], "wb"))

for line in io.stdin:lines() do
	-- Skip Lua comments
	if not line:match("^%s*%-%-") then
		local rec = string.pack("<!1 I2 I1 I1 I4", load_insn(line))
		out:write(rec)
	end
end

out:close()
