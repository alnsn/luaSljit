-- bpf module.
local bpf = {
	memwords    = 16,
	maxmemwords = 30,
	maxpktlen   = 0xffffffff,
}

-- Namespace for all BPF_XXX constants, e.g. bpf.C.RET is 0x06.
-- Constants are also available at module's level, e.g. bpf.RET.
bpf.C = {}

-- Namespaces for constant names. It's more complicated
-- than bpf.C because some constants have duplicate values.
-- Those namespaces are initialised by macrodef().
-- Some examples:
-- bpf.S.class[0x04] == "ALU",
-- bpf.S.class[0x05] == "JMP",
-- bpf.S.alu.op[0x10] == "SUB" and bpf.S[0x04].op[0x10] == "SUB",
-- bpf.S.jmp.op[0x10] == "JEQ" and bpf.S[0x05].op[0x10] == "JEQ".
bpf.S = {
	class = {},                   -- BPF_CLASS() constants
	ld    = { size={}, mode={} }, -- BPF_LD   class
	ldx   = { size={}, mode={} }, -- BPF_LDX  class
	st    = {},                   -- BPF_ST   class
	stx   = {},                   -- BPF_STX  class
	alu   = { src={}, op={} },    -- BPF_ALU  class
	jmp   = { src={}, op={} },    -- BPF_JMP  class
	ret   = { rval={} },          -- BPF_RET  class
	misc  = { miscop={} },        -- BPF_MISC class
	-- Numeric keys will be added later by macrodef() and
	-- they will refer to the tables listed above.
}

-- Namespaces for methods.
-- For instance:
-- insn:class() -> bpf.M.class(insn),
-- insn:jumps() -> bpf.M.jmp.jumps(insn).
bpf.M = {
	ld    = {}, -- BPF_LD   class
	ldx   = {}, -- BPF_LDX  class
	st    = {}, -- BPF_ST   class
	stx   = {}, -- BPF_STX  class
	alu   = {}, -- BPF_ALU  class
	jmp   = {}, -- BPF_JMP  class
	ret   = {}, -- BPF_RET  class
	misc  = {}, -- BPF_MISC class
}

-- Compat stuff
if not bit32 then bit32 = bit end
if not bit32 then bit32 = require "bit" end

local band = bit32.band
local tinsert = table.insert
local sformat = string.format

-- No need for a shortcut because string.lower is only
-- required while loading the module.
--local slower  = string.lower


-- Registrar of BPF_CLASS(), BPF_SIZE(), etc macros and their codes.
local function macrodef(spec)
	local macro_spec = spec[1]
	local macro,class = macro_spec:match("(%w+):(%w+)")

	macro = macro or macro_spec

	local mask = spec[2]
	local macro_mask = macro .. "_mask"

	assert(not bpf[macro_mask] or bpf[macro_mask] == mask,
	    "bpf." .. macro_mask .. " value can't be changed")

	bpf[macro_mask] = mask
	bpf[macro] = function(code) return band(code, mask) end

	local function method(insn)
		return band(insn.code, mask)
	end 

	-- Copy codes to avoid an extra indentation in the big loop below.
	local codes = {}
	for classname,code in pairs(spec) do
		if type(classname) ~= "number" then
			codes[classname] = code
		end
	end

	local C,M,S = bpf.C,bpf.M,bpf.S

	for classname,code in pairs(codes) do
		assert(not C[classname] or C[classname] == code,
		    "bpf.C." .. classname .. " value can't be changed")

		C[classname] = code
		bpf[classname] = code

		if macro == "class" then
			-- Add a numeric key to bpf.S.
			local namespace = S[string.lower(classname)]
			S[code] = assert(namespace, "bad class " ..
			    classname .. " or missing entry in bpf.S")
			S.class[code] = classname
			M.class = method
		elseif class then
			-- There is only one matching class.
			S[class][macro][code] = classname
			M[class][macro] = method
		else
			-- Initialise S[*][macro][code] and
			-- M[*][macro] for all matching classes.
			for class,namespace in pairs(S) do
				if namespace[macro] and
				    type(class) == "string" then
					namespace[macro][code] = classname
					M[class][macro] = method
				end
			end
		end
	end
end

-- BPF_CLASS()
macrodef {
	"class", 0x07;
	LD   = 0x00,
	LDX  = 0x01,
	ST   = 0x02,
	STX  = 0x03,
	ALU  = 0x04,
	JMP  = 0x05,
	RET  = 0x06,
	MISC = 0x07,
}

-- BPF_SIZE()
macrodef {
	"size", 0x18;
	W = 0x00,
	H = 0x08,
	B = 0x10,
}

-- BPF_MODE()
macrodef {
	"mode:ld", 0xe0;
	IMM = 0x00,
	ABS = 0x20,
	IND = 0x40,
	MEM = 0x60,
	LEN = 0x80,
}
macrodef {
	"mode:ldx", 0xe0;
	IMM = 0x00,
	MEM = 0x60,
	LEN = 0x80,
	MSH = 0xa0,
}

-- BPF_OP()
macrodef {
	"op:alu", 0xf0;
	ADD  = 0x00,
	SUB  = 0x10,
	MUL  = 0x20,
	DIV  = 0x30,
	OR   = 0x40,
	AND  = 0x50,
	LSH  = 0x60,
	RSH  = 0x70,
	NEG  = 0x80,
	MOD  = 0x90,
	XOR  = 0xa0,
}
macrodef {
	"op:jmp", 0xf0;
	JA   = 0x00,
	JEQ  = 0x10,
	JGT  = 0x20,
	JGE  = 0x30,
	JSET = 0x40,
}

-- BPF_SRC()
macrodef {
	"src", 0x08;
	K = 0x00,
	X = 0x08,
}

-- BPF_RVAL()
macrodef {
	"rval", 0x18;
	K = 0x00,
	A = 0x10,
}

-- BPF_MISCOP()
macrodef {
	"miscop", 0xf8;
	TAX  = 0x00,
	COP  = 0x20,
	COPX = 0x40,
	TXA  = 0x80,
}


-- Helpers.

local class_mask  = bpf.class_mask
local size_mask   = bpf.size_mask
local mode_mask   = bpf.mode_mask
local op_mask     = bpf.op_mask
local src_mask    = bpf.src_mask
local rval_mask   = bpf.rval_mask
local miscop_mask = bpf.miscop_mask

-- Inputs, outputs and inouts of all instructions.
local NO_inout = {                }
local A_inout  = { A=true         }
local M_inout  = { M=true         }
local P_inout  = { P=true         }
local X_inout  = { X=true         }
local AX_inout = { A=true, X=true }
local PX_inout = { P=true, X=true }
local inout_specs = {
	[bpf.ALU+bpf.ADD+bpf.K]  = { A_inout,  A_inout,  A_inout  },
	[bpf.ALU+bpf.ADD+bpf.X]  = { AX_inout, A_inout,  A_inout  },
	[bpf.ALU+bpf.AND+bpf.K]  = { A_inout,  A_inout,  A_inout  },
	[bpf.ALU+bpf.AND+bpf.X]  = { AX_inout, A_inout,  A_inout  },
	[bpf.ALU+bpf.DIV+bpf.K]  = { A_inout,  A_inout,  A_inout  },
	[bpf.ALU+bpf.DIV+bpf.X]  = { AX_inout, A_inout,  A_inout  },
	[bpf.ALU+bpf.LSH+bpf.K]  = { A_inout,  A_inout,  A_inout  },
	[bpf.ALU+bpf.LSH+bpf.X]  = { AX_inout, A_inout,  A_inout  },
	[bpf.ALU+bpf.MOD+bpf.K]  = { A_inout,  A_inout,  A_inout  },
	[bpf.ALU+bpf.MOD+bpf.X]  = { AX_inout, A_inout,  A_inout  },
	[bpf.ALU+bpf.MUL+bpf.K]  = { A_inout,  A_inout,  A_inout  },
	[bpf.ALU+bpf.MUL+bpf.X]  = { AX_inout, A_inout,  A_inout  },
	[bpf.ALU+bpf.NEG]        = { A_inout,  A_inout,  A_inout  },
	[bpf.ALU+bpf.OR+bpf.K]   = { A_inout,  A_inout,  A_inout  },
	[bpf.ALU+bpf.OR+bpf.X]   = { AX_inout, A_inout,  A_inout  },
	[bpf.ALU+bpf.RSH+bpf.K]  = { A_inout,  A_inout,  A_inout  },
	[bpf.ALU+bpf.RSH+bpf.X]  = { AX_inout, A_inout,  A_inout  },
	[bpf.ALU+bpf.SUB+bpf.K]  = { A_inout,  A_inout,  A_inout  },
	[bpf.ALU+bpf.SUB+bpf.X]  = { AX_inout, A_inout,  A_inout  },
	[bpf.ALU+bpf.XOR+bpf.K]  = { A_inout,  A_inout,  A_inout  },
	[bpf.ALU+bpf.XOR+bpf.X]  = { AX_inout, A_inout,  A_inout  },
	[bpf.JMP+bpf.JA]         = { NO_inout, NO_inout, NO_inout },
	[bpf.JMP+bpf.JEQ+bpf.K]  = { A_inout,  NO_inout, NO_inout },
	[bpf.JMP+bpf.JEQ+bpf.X]  = { AX_inout, NO_inout, NO_inout },
	[bpf.JMP+bpf.JGE+bpf.K]  = { A_inout,  NO_inout, NO_inout },
	[bpf.JMP+bpf.JGE+bpf.X]  = { AX_inout, NO_inout, NO_inout },
	[bpf.JMP+bpf.JGT+bpf.K]  = { A_inout,  NO_inout, NO_inout },
	[bpf.JMP+bpf.JGT+bpf.X]  = { AX_inout, NO_inout, NO_inout },
	[bpf.JMP+bpf.JSET+bpf.K] = { A_inout,  NO_inout, NO_inout },
	[bpf.JMP+bpf.JSET+bpf.X] = { AX_inout, NO_inout, NO_inout },
	[bpf.LD+bpf.B+bpf.ABS]   = { P_inout,  A_inout,  NO_inout },
	[bpf.LD+bpf.B+bpf.IND]   = { PX_inout, A_inout,  NO_inout },
	[bpf.LD+bpf.H+bpf.ABS]   = { P_inout,  A_inout,  NO_inout },
	[bpf.LD+bpf.H+bpf.IND]   = { PX_inout, A_inout,  NO_inout },
	[bpf.LD+bpf.W+bpf.ABS]   = { P_inout,  A_inout,  NO_inout },
	[bpf.LD+bpf.W+bpf.IMM]   = { NO_inout, A_inout,  NO_inout },
	[bpf.LD+bpf.W+bpf.IND]   = { PX_inout, A_inout,  NO_inout },
	[bpf.LD+bpf.W+bpf.LEN]   = { NO_inout, A_inout,  NO_inout },
	[bpf.LD+bpf.W+bpf.MEM]   = { M_inout,  A_inout,  NO_inout },
	[bpf.LDX+bpf.B+bpf.MSH]  = { P_inout,  X_inout,  NO_inout },
	[bpf.LDX+bpf.W+bpf.IMM]  = { NO_inout, X_inout,  NO_inout },
	[bpf.LDX+bpf.W+bpf.LEN]  = { NO_inout, X_inout,  NO_inout },
	[bpf.LDX+bpf.W+bpf.MEM]  = { M_inout,  X_inout,  NO_inout },
	[bpf.MISC+bpf.COPX]      = { AX_inout, A_inout,  A_inout  },
	[bpf.MISC+bpf.COP]       = { A_inout,  A_inout,  A_inout  },
	[bpf.MISC+bpf.TAX]       = { A_inout,  X_inout,  NO_inout },
	[bpf.MISC+bpf.TXA]       = { X_inout,  A_inout,  NO_inout },
	[bpf.RET+bpf.A]          = { A_inout,  NO_inout, NO_inout },
	[bpf.RET+bpf.K]          = { NO_inout, NO_inout, NO_inout },
	[bpf.STX]                = { X_inout,  M_inout,  NO_inout },
	[bpf.ST]                 = { A_inout,  M_inout,  NO_inout },
}

-- To substitute M=true in inout_specs with M={[k]=true},
-- where k is a memword index.
local inout_Mspecs = {}
for n = 1, bpf.maxmemwords do
	inout_Mspecs[n] = { M = { [n-1] = true } }
end

-- Check that inouts are intersections of inputs and outputs.
--for _,spec in pairs(inout_specs) do
--	for k,_ in pairs(spec[1]) do
--		assert(spec[2][k] == spec[3][k])
--	end
--
--	for k,_ in pairs(spec[2]) do
--		assert(spec[1][k] == spec[3][k])
--	end
--
--	for k,_ in pairs(spec[3]) do
--		assert(spec[1][k] and spec[2][k])
--	end
--end

local pktread_widths = {
	[bpf.LD+bpf.B+bpf.ABS]   = 1,
	[bpf.LD+bpf.B+bpf.IND]   = 1,
	[bpf.LD+bpf.H+bpf.ABS]   = 2,
	[bpf.LD+bpf.H+bpf.IND]   = 2,
	[bpf.LD+bpf.W+bpf.ABS]   = 4,
	[bpf.LD+bpf.W+bpf.IND]   = 4,
	[bpf.LDX+bpf.B+bpf.MSH]  = 1,
}

-- Specs for formatted strings.
local strspecs = {
	[bpf.ALU+bpf.ADD+bpf.K]  = { "add", "#%d"           },
	[bpf.ALU+bpf.ADD+bpf.X]  = { "add", "x"             },
	[bpf.ALU+bpf.AND+bpf.K]  = { "and", "#0x%x"         },
	[bpf.ALU+bpf.AND+bpf.X]  = { "and", "x"             },
	[bpf.ALU+bpf.DIV+bpf.K]  = { "div", "#%d"           },
	[bpf.ALU+bpf.DIV+bpf.X]  = { "div", "x"             },
	[bpf.ALU+bpf.LSH+bpf.K]  = { "lsh", "#%d"           },
	[bpf.ALU+bpf.LSH+bpf.X]  = { "lsh", "x"             },
	[bpf.ALU+bpf.MOD+bpf.K]  = { "mod", "#%d"           },
	[bpf.ALU+bpf.MOD+bpf.X]  = { "mod", "x"             },
	[bpf.ALU+bpf.MUL+bpf.K]  = { "mul", "#%d"           },
	[bpf.ALU+bpf.MUL+bpf.X]  = { "mul", "x"             },
	[bpf.ALU+bpf.NEG]        = { "neg", ""              },
	[bpf.ALU+bpf.OR+bpf.K]   = { "or", "#0x%x"          },
	[bpf.ALU+bpf.OR+bpf.X]   = { "or", "x"              },
	[bpf.ALU+bpf.RSH+bpf.K]  = { "rsh", "#%d"           },
	[bpf.ALU+bpf.RSH+bpf.X]  = { "rsh", "x"             },
	[bpf.ALU+bpf.SUB+bpf.K]  = { "sub", "#%d"           },
	[bpf.ALU+bpf.SUB+bpf.X]  = { "sub", "x"             },
	[bpf.ALU+bpf.XOR+bpf.K]  = { "xor", "#0x%x"         },
	[bpf.ALU+bpf.XOR+bpf.X]  = { "xor", "x"             },
	[bpf.JMP+bpf.JA]         = { "ja", "%d"             },
	[bpf.JMP+bpf.JEQ+bpf.K]  = { "jeq", "#0x%x"         },
	[bpf.JMP+bpf.JEQ+bpf.X]  = { "jeq", "x"             },
	[bpf.JMP+bpf.JGE+bpf.K]  = { "jge", "#0x%x"         },
	[bpf.JMP+bpf.JGE+bpf.X]  = { "jge", "x"             },
	[bpf.JMP+bpf.JGT+bpf.K]  = { "jgt", "#0x%x"         },
	[bpf.JMP+bpf.JGT+bpf.X]  = { "jgt", "x"             },
	[bpf.JMP+bpf.JSET+bpf.K] = { "jset", "#0x%x"        },
	[bpf.JMP+bpf.JSET+bpf.X] = { "jset", "x"            },
	[bpf.LD+bpf.B+bpf.ABS]   = { "ldb", "[%d]"          },
	[bpf.LD+bpf.B+bpf.IND]   = { "ldb", "[x + %d]"      },
	[bpf.LD+bpf.H+bpf.ABS]   = { "ldh", "[%d]"          },
	[bpf.LD+bpf.H+bpf.IND]   = { "ldh", "[x + %d]"      },
	[bpf.LD+bpf.W+bpf.ABS]   = { "ld" , "[%d]"          },
	[bpf.LD+bpf.W+bpf.IMM]   = { "ld", "#0x%x"          },
	[bpf.LD+bpf.W+bpf.IND]   = { "ld", "[x + %d]"       },
	[bpf.LD+bpf.W+bpf.LEN]   = { "ld", "#pktlen"        },
	[bpf.LD+bpf.W+bpf.MEM]   = { "ld", "M[%d]"          },
	[bpf.LDX+bpf.B+bpf.MSH]  = { "ldxb", "4*([%d]&0xf)" },
	[bpf.LDX+bpf.W+bpf.IMM]  = { "ldx", "#0x%x"         },
	[bpf.LDX+bpf.W+bpf.LEN]  = { "ldx", "#pktlen"       },
	[bpf.LDX+bpf.W+bpf.MEM]  = { "ldx", "M[%d]"         },
	[bpf.MISC+bpf.COPX]      = { "cop", "[x]"           },
	[bpf.MISC+bpf.COP]       = { "cop", "[%d]"          },
	[bpf.MISC+bpf.TAX]       = { "tax", ""              },
	[bpf.MISC+bpf.TXA]       = { "txa", ""              },
	[bpf.RET+bpf.A]          = { "ret"                  },
	[bpf.RET+bpf.K]          = { "ret", "#%d"           },
	[bpf.STX]                = { "stx", "M[%d]"         },
	[bpf.ST]                 = { "st", "M[%d]"          },
}

local function table_keys(t)
	local k, v

	local function iter()
		k,v = next(t, k)
		return k
	end

	return iter
end

-- XXX not used
local function table_values(t)
	local k, v

	local function iter()
		k,v = next(t, k)
		return v
	end

	return iter
end

local function table_ivalues(t)
	local n = 0

	local function iter()
		n = n + 1
		return t[n]
	end

	return iter
end


-- Module's functions, part I.

-- XXX bpf.codes(class)
function bpf.codes()
	local res = {}

	for code in table_keys(inout_specs) do
		tinsert(res, code)
	end

	return res
end

-- XXX bpf.codes_iter(class)
function bpf.codes_iter()
	return table_keys(inout_specs)
end


-- Methods.

-- Check whether insn is a load from packet data. It returns
-- a width of the read (4 for bpf.W, 2 for bpf.H and 1 for
-- bpf.B) as a side effect.
function bpf.M.pktread(insn)
	return pktread_widths[insn.code]
end

-- If insn:pktread(), return a mininum length of a packet required by
-- the insn to successfully read packet data. Otherwise, return 0.
function bpf.M.pktlen(insn)
	local width = pktread_widths[insn.code]
	return width and insn.k + width or 0
end

local function inout_table(insn, inout_ind)
	local res = inout_specs[insn.code][inout_ind]
	return res == M_inout and inout_Mspecs[insn.k + 1] or res
end

function bpf.M.inputs(insn)
	return inout_table(insn, 1)
end

function bpf.M.inputs_iter(insn)
	return table_keys(inout_table(insn, 1))
end

function bpf.M.outputs(insn)
	return inout_table(insn, 2)
end

function bpf.M.outputs_iter(insn)
	return table_keys(inout_table(insn, 2))
end

function bpf.M.inouts(insn)
	return inout_table(insn, 3)
end

function bpf.M.inouts_iter(insn)
	return table_keys(inout_table(insn, 3))
end

local function jumps(insn, n)
	-- assert(band(insn.code, class_mask) == bpf.JMP)
	local op = bpf.op(insn.code)
	n = n or -1

	if op == bpf.JA then
		return { n + 1 + insn.k }
	end

	local jt,jf = insn.jt, insn.jf

	if jt == jf then
		return { n + 1 + jt }
	end

	return { n + 1 + jt, n + 1 + jf }
end

local function hops(insn, n)
	local class = band(insn.code, class_mask)

	if class == bpf.RET then
		return {}
	elseif class ~= bpf.JMP then
		return { n and n + 1 or 0 }
	else
		return jumps(insn, n)
	end
end

function bpf.M.hops(insn, n)
	return hops(insn, n)
end

function bpf.M.hops_iter(insn, n)
	return table_ivalues(hops(insn, n))
end

function bpf.M.jmp.jumps(insn, n)
	return jumps(insn, n)
end

function bpf.M.jmp.jumps_iter(insn, n)
	return table_ivalues(jumps(insn, n))
end

-- Return numeric return value or "A".
function bpf.M.ret.retval(insn)
	-- assert(band(insn.code, class_mask) == bpf.RET)
	return band(insn.code, rval_mask) == bpf.A and "A" or insn.k
end

-- Check whether insn is "return 0" instruction.
function bpf.M.ret.ret0(insn)
	-- assert(band(insn.code, class_mask) == bpf.RET)
	return band(insn.code, rval_mask) == bpf.K and insn.k == 0
end

-- Convert @insn to C or Lua language expression.
-- @how "C", "L", "C|", "C+" or "L+".
local function toexpr(insn, how)
	local code = insn.code
	local class = band(code, class_mask)
	local lang = how:sub(1, 1)
	local delim = how:sub(2, 2)
	local lsyntax = lang ~= "C"
	local constructor = lsyntax and "bpf.stmt(" or
	    class == bpf.JMP and "BPF_JUMP(" or "BPF_STMT("
	local const_prefix = lsyntax and "bpf." or "BPF_"

	delim = #delim > 0 and delim or "+"

	assert(delim == "+" or delim == "|" and lang == "C",
	    "invalid delimiter " .. delim)

	local S = bpf.S
	local Sclass = S[class]
	local res = constructor .. const_prefix .. S.class[class]

	for macro,_ in pairs(Sclass) do
		local macroval = bpf[macro](code) -- e.g., bpf.miscop(code)
		local macrovalstr = Sclass[macro][macroval]
		res = res .. delim .. (macrovalstr and const_prefix ..
		    macrovalstr or string.format("0x%x", macroval))
	end

	res = sformat("%s, %d", res, insn.k)

	if class == bpf.JMP and bpf.op(code) ~= bpf.JA then
		res = sformat("%s, %d, %d", res, insn.jt, insn.jf)
	end

	return res .. ")"
end

-- Santity check of strspecs.
--for code in bpf.codes_iter() do assert(strspecs[code]) end

-- Strings returned by this functions are similar
-- to tcpdump -d <rule> output.
local function tostr(insn, n)
	local compact = not n
	n = compact and -1 or n

	local code = insn.code
	local spec = strspecs[code]
	local insnstr = spec and spec[1] or "unimp"
	local format  = spec and spec[2] or "0x%x"
	local member  = spec and "k" or "code"
	local class = band(code, class_mask)
	local jump = class == bpf.JMP
	local ja = jump and band(code, op_mask) == bpf.JA
	local jw = compact and "%d" or "%03d"
	local jfmt = jump and not ja and
	    " jt " .. jw .. " jf " .. jw or ""

	local insnarg = sformat(format, insn[member] + (ja and n + 1 or 0))
	local joffset = jump and n + 1 or 0

	if compact then
		return sformat("%s %s" .. jfmt,
		    insnstr, insnarg, insn.jt + joffset, insn.jf + joffset)
	else
		return sformat("(" .. jw .. ") %-8s %-16s" .. jfmt,
		    n, insnstr, insnarg, insn.jt + joffset, insn.jf + joffset)
	end
end

function bpf.M.tostring(insn, arg)
	return type(arg) == "string" and toexpr(insn, arg) or tostr(insn, arg)
end

local metatables = {}

-- @class Constant from bpf.C namespace.
local function classdef(class)
	local M = bpf.M
	local methods = {}

	-- Copy common methods from bpf.M.
	for k,v in pairs(M) do
		if type(v) == "function" then
			methods[k] = v
		end
	end

	-- Copy class-specific methods.
	local classname = string.lower(bpf.S.class[class])
	for k,v in pairs(M[classname]) do
		methods[k] = v
	end

	metatables[class] = { __index=methods, __tostring=tostr }
end

classdef(bpf.C.LD)
classdef(bpf.C.LDX)
classdef(bpf.C.ST)
classdef(bpf.C.STX)
classdef(bpf.C.ALU)
classdef(bpf.C.JMP)
classdef(bpf.C.RET)
classdef(bpf.C.MISC)

-- Module's functions, part II.

-- BPF_STMT(), extended to initialise BPF_JMP class.
function bpf.stmt(code, k, jt, jf)
	local class = band(code, class_mask)
	assert(inout_specs[code], "invalid code")
	local res = { code = code, k = k, jt = jt or 0, jf = jf or 0 }
	return setmetatable(res, metatables[class])
end

-- BPF_JUMP()
function bpf.jump(code, k, jt, jf)
	local class = band(code, class_mask)
	assert(inout_specs[code] and class == bpf.JMP, "invalid code")
	local res = { code = code, k = k, jt = jt, jf = jf }
	return setmetatable(res, metatables[class])
end

return bpf
