#!/usr/bin/env lua5.3
-- Read filter programs from binary files and dump those programs
-- to human readable format similar to an output of tcpdump -dd.

-- Setting it to true might help to repair some malformed instructions.
local repair = false

local bpf = require "bpf"
local args = {...}

for _,filename in ipairs(args) do
	local file = assert(io.open(filename))
	local data = file:read("a")
	file:close()

	print(string.format("-- %q", filename))

	local pos = 1
	while pos < #data do
		local code, jt, jf, k, newpos =
		    string.unpack("<!1 I2 I1 I1 I4", data, pos)
		pos = newpos

		local valid, insn = pcall(bpf.stmt, code, k, jt, jf)

		if repair and not valid then
			local t = { code=code, jt=jt, jf=jf, k=k }
			local converter = load("local bpf=...;return " ..
			    bpf.M.tostring(t, "L"))
			local valid, insn =  pcall(converter, bpf)
			if valid then
				code = insn.code
				jt = insn.jt
				jf = insn.jf
				k = insn.k
			end
		end

		local insnstr = valid and tostring(insn) or
		    bpf.M.tostring({ code=code, jt=jt, jf=jf, k=k }, "L")
		print(string.format("{0x%x, %d, %d, 0x%x}\t--\t%s",
		    code, jt, jf, k, insnstr))
	end
end
