local bpf = require "bpf"

local tinsert   = table.insert
local maxpktlen = bpf.maxpktlen
local maxabclen = maxpktlen + 4 -- Max. read width is 4 (BPF_W).

-- bpf.opt module.
local bpfopt = {}

-- Set elements array[n][member] greater than to threshold to threshold.
local function lower_elements(threshold, array, member)
	for _,t in ipairs(array) do
		if t[member] > threshold then
			t[member] = threshold
		end
	end
end

-- Find the min. element among array[n][member] and res.
local function min_element(res, array, member)
	for _,t in ipairs(array) do
		local v = t[member]
		if res > v then
			res = v
		end
	end

	return res
end

local function reachable_seq(prog)
	local function iter(prog, insn)
		repeat
			insn = prog[insn.label + 1]
		until not insn or not insn.dead

		return insn
	end

	return iter, prog, { label = 0 }
end

local function reachable_reverse_seq(prog)
	local function iter(prog, insn)
		repeat
			insn = prog[insn.label - 1]
		until not insn or not insn.dead

		return insn
	end

	return iter, prog, { label = #prog + 1 }
end

local function init_prog(prog)
	for n,insn in ipairs(prog) do
		insn.label = nil
		insn.dead = nil
		insn.check_length = insn:pktlen()

		-- XXX Use a separate array prog_shadow for temporary fields.
		insn.jump_list = nil
		insn.checked_length = nil
		insn.abc_length = insn:class() == bpf.JMP and maxabclen or nil
	end
end

local function init_flow(prog)
	local dead = false

	for n,insn in ipairs(prog) do
		if insn.jump_list then
			dead = false
		end

		insn.label = n
		insn.dead = dead

		local hops = insn:hops(n)

		if dead or #hops == 0 or insn:pktlen() > maxpktlen then
			dead = true
		elseif insn:class() == bpf.JMP then
			local jt = hops[1]
			local jf = hops[2] or jt

			dead = jt > n + 1 and jf > n + 1

			jt = prog[jt]
			jf = prog[jf]

			assert(jt and jf, "invalid jump")

			jt.jump_list = jt.jump_list or {}
			tinsert(jt.jump_list, insn)

			if hops[2] then
				jf.jump_list = jf.jump_list or {}
				tinsert(jf.jump_list, insn)
			end
		end
	end
end

local function init_abc(prog)
	local abc_length = 0

	for insn in reachable_reverse_seq(prog) do
		local class = insn:class()

		if class == bpf.JMP then
			abc_length = insn.abc_length
		elseif class == bpf.RET then
			-- XXX comment
			abc_length = insn:ret0() and maxabclen or 0
		elseif class == bpf.MISC then
			local miscop = insn:miscop()
			if miscop == bpf.COP or miscop == bpf.COPX then
				abc_length = 0 -- cop may have side effects
			end
		end
		-- XXX external memory

		local pktlen = insn:pktlen()
		if pktlen > 0 then
			if abc_length < pktlen then
				abc_length = pktlen
			end
			insn.abc_length = abc_length
		end

		if insn.jump_list then
			lower_elements(abc_length,
			    insn.jump_list, "abc_length")
		end
	end

	local checked_length = 0

	for insn in reachable_seq(prog) do
		if insn.jump_list then
			checked_length = min_element(checked_length,
			    insn.jump_list, "checked_length")
		end

		if insn:class() == bpf.JMP then
			insn.checked_length = checked_length
		elseif insn:pktread() then
			local check = checked_length < insn.abc_length
			if check then
				checked_length = insn.abc_length
			end
			insn.check_length = check and checked_length or 0
		end
	end
end

function bpfopt.optimise(prog)
	init_prog(prog)
	init_flow(prog)
	init_abc(prog)
end

return bpfopt
