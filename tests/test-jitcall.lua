local sljit = require "sljit"
local jitcall = require "jitcall"

-- Magic numbers for x/17.
local mul, sh1, sh2 = 0xe1e1e1e2, 1, 4

local compiler = sljit.create_compiler()

compiler:emit_enter{args=1, saveds=1, scratches=1}
    :emit_op2('MUL',   'R0', 'S0', sljit.imm(mul))
    :emit_op2('LSHR',  'R0', 'R0', sljit.imm(32))
    :emit_op2('ISUB',  'S0', 'S0', 'R0')
    :emit_op2('ILSHR', 'S0', 'S0', sljit.imm(sh1))
    :emit_op2('IADD',  'R0', 'R0', 'S0')
    :emit_op2('ILSHR', 'R0', 'R0', sljit.imm(sh2))

-- Pick any line you like. Or don't. Two returns don't make any harm.
compiler:emit_return('MOV_UI', 'R0', 0)
sljit.emit_return(compiler, 'MOV', 'R0', 0)

local code = compiler:generate_code()

local z = 17 * 16 + 8
print(string.format("%d / 17 = %d", z, jitcall.call(code, z)))
