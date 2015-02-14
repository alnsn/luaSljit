#!/usr/bin/env lua5.3
-- Convert an output of tcpdump -dd to a digraph.

-- Local tweaks
local ft = {false, true} -- to enable vim shortcuts
local rankdir   = "TB" -- TB LR
local linear    = ft[2]
local optimise  = ft[2]
local add_ret0  = ft[2]
local show_ret0 = ft[2]
local font = "Courier-Bold"
local node_attributes      = { fontname=font, fontsize=14 }
local insn_node_attributes = { fontname=font, fontsize=20 }
local edge_attributes      = { fontname=font, fontsize=14, penwidth=2 }
local ret0_edge_attributes = { fontname=font, fontsize=14, penwidth=2 }

local bpf = require "bpf"
local bpfopt = require "bpf.opt"

-- Compat stuff
if not bit32 then bit32 = bit end
if not bit32 then bit32 = require "bit" end
if not loadstring then loadstring = load end

-- Convert one line of tcpdump -dd, e.g. "{ 0x15, 3, 4, 0x00000006 },"
-- to lua table { 0x15, k=0x15, 3, jt=3, 4, jf=4, 0x06, code=0x06 }.
-- WARNING this function evaluates the input string!
local function load_insn(line)
	local chunk = assert(loadstring("return " .. line .. "nil"))
	local insn = chunk() -- eval
	return bpf.stmt(insn[1], insn[4], insn[2], insn[3])
end

local function parse_dot_attributes(attributes, ...)
	attributes = attributes:format(...)
	local res = {}
	for attr,value in attributes:gmatch("(%w+)=([^;]*);?") do
		res[attr] = value
	end
	return res
end

local function merge_dot_attributes(attributes1, attributes2, ...)
	if type(attributes1) == "string" then
		attributes1 = parse_dot_attributes(attributes1)
	end

	if type(attributes2) == "string" then
		attributes2 = parse_dot_attributes(attributes2, ...)
	end

	for attr,value in pairs(attributes2) do
		attributes1[attr] = value
	end

	return attributes1
end

local function format_dot_attributes(attributes)
	local res = ""
	for attr,value in pairs(attributes) do
		res = string.format("%s%s=%q;", res, attr, tostring(value))
	end
	return res:sub(1, -2)
end

local function join_dot_attributes(attributes1, attributes2, ...)
	local merged = merge_dot_attributes(attributes1, attributes2, ...)
	return format_dot_attributes(merged)
end

local labels = { "true", "false" }
local label_colors = { "blue", "red" }

local prog = {}
for line in io.stdin:lines() do
	table.insert(prog, load_insn(line))
end

if optimise then
	bpfopt.optimise(prog)
end

local ret0 = false
local last_insn = prog[#prog]
local ret0_edge = (last_insn and last_insn:class() == bpf.RET and
    last_insn:ret0()) and #prog or #prog + 1

print(string.format("digraph {\nrankdir=%q;", rankdir))

if linear then
	for n = 1,#prog do
		print(string.format("T%d->T%d[style=invis];", n, n+1))
	end

	for n,insn in ipairs(prog) do
		local attributes = {
			shape = "plaintext",
			label = insn:tostring(),
			fontcolor = insn.unreachable and "grey" or "black"
		}
		print(string.format("T%d[%s];", n,
		    join_dot_attributes(insn_node_attributes, attributes)))
	end
	
	print(string.format("T%d[style=invis;label=end];", #prog+1))
end

for n,insn in ipairs(prog) do
	local class = insn:class()
	local hops  = insn:hops(n)

	for i = 1,#hops do
		local attributes = {
			color = hops[2] and label_colors[i] or "black"
		}
		print(string.format("N%d->N%d[%s];", n, hops[i],
		    join_dot_attributes(edge_attributes, attributes)))
	end

	local pktlen = insn:pktlen()
	local pktread = pktlen > 0

	if add_ret0 and pktread then
		ret0 = true
		local ind = insn:class() == bpf.LD and insn:mode() == bpf.IND
		local check = not optimise or insn.check_length > 0 or ind
		local attributes = {
			style = (show_ret0 and check) and "dotted" or "invis",
			--taillabel = string.format("pktlen<%d",
			--    insn.check_length or pktlen)
		}
		print(string.format("N%d->N%d[%s];", n, ret0_edge,
		    join_dot_attributes(ret0_edge_attributes, attributes)))
	end

	local attributes = {
		label = string.format(pktread and "%s|%d" or "%s",
		    insn:tostring(), insn.check_length or pktlen),
		color = insn.unreachable and "grey" or "black",
		shape = #hops > 1 and "diamond" or pktread and
		    "Mrecord" or "ellipse"
	}
	print(string.format("N%d[%s];", n,
	    join_dot_attributes(node_attributes, attributes)))

	if linear then
		print(string.format("{rank=same; N%d; T%d}", n, n))
	end
end

if ret0 and not prog[ret0_edge] then
	print(string.format("N%d[shape=ellipse;label=%q;color=grey];",
	    ret0_edge, "ret #0"))
	if linear then
		print(string.format("{rank=same; N%d; T%d}",
		    ret0_edge, #prog+1))
	end
end

print "}"
