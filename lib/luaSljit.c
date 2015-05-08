/*-
 * Copyright (c) 2013-2015 Alexander Nasonov.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include "luaSljit.h"

#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>

#include <sljitLir.h>

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

/* Metatables. */
#define CODE_METATABLE  "sljit.code"
#define COMP_METATABLE  "sljit.compiler"
#define CONST_METATABLE "sljit.const"
#define JUMP_METATABLE  "sljit.jump"
#define LABEL_METATABLE "sljit.label"
#define ARG_METATABLE   "sljit.argument"

/* Indices in uservalue table */
#define COMPILER_UVAL_INDEX 1

/* Errors. */
#define ERR_NOCONV(type) "conversion to " type " failed"

typedef int constant_flag_t;
#define TYPE_NOTUD 0 /* Not luaSljitArg userdata. */
#define TYPE_REG 1
#define TYPE_OP0 2
#define TYPE_OP1 3
#define TYPE_OP2 4
#define TYPE_CMP 5
#define TYPE_MASK 15

/* TYPE_REG flags. */
#define REG_IMM  0
#define REG_ONLY 16
#define REG_MASK 16

/* TYPE_OP1 flags. */
#define OP1_RET 32 /* can be passed to sljit_emit_return() */

/* TYPE_CMP flags. */
#define CMP_JMP 64

/* Userdata for SLJIT_XXX constants. */
struct luaSljitArg
{
	constant_flag_t flags;
	sljit_si argi; // src, dst or op
	sljit_sw argw; // srcw or dstw
};

struct constant
{
	const char *name;
	struct luaSljitArg arg;
};

#define DEFCONST(name, flags) { #name, { (flags), SLJIT_##name, 0 } }

static const struct constant constants[] = {

	/* XXX more of these */
	DEFCONST(NUMBER_OF_REGISTERS,       TYPE_NOTUD),
	DEFCONST(NUMBER_OF_SAVED_REGISTERS, TYPE_NOTUD),

	/* General purpose registers. */
	/* XXX PREF_SHIFT_REG R2, RETURN_REG R0 */
	DEFCONST(S0,     TYPE_REG|REG_ONLY),
	DEFCONST(S1,     TYPE_REG|REG_ONLY),
	DEFCONST(S2,     TYPE_REG|REG_ONLY),
	DEFCONST(S3,     TYPE_REG|REG_ONLY),
	DEFCONST(S4,     TYPE_REG|REG_ONLY),
	DEFCONST(S5,     TYPE_REG|REG_ONLY),
	DEFCONST(S6,     TYPE_REG|REG_ONLY),
	DEFCONST(S7,     TYPE_REG|REG_ONLY),
	DEFCONST(S8,     TYPE_REG|REG_ONLY),
	DEFCONST(S9,     TYPE_REG|REG_ONLY),
	DEFCONST(R0,     TYPE_REG|REG_ONLY),
	DEFCONST(R1,     TYPE_REG|REG_ONLY),
	DEFCONST(R2,     TYPE_REG|REG_ONLY),
	DEFCONST(R3,     TYPE_REG|REG_ONLY),
	DEFCONST(R4,     TYPE_REG|REG_ONLY),
	DEFCONST(R5,     TYPE_REG|REG_ONLY),
	DEFCONST(R6,     TYPE_REG|REG_ONLY),
	DEFCONST(R7,     TYPE_REG|REG_ONLY),
	DEFCONST(R8,     TYPE_REG|REG_ONLY),
	DEFCONST(R9,     TYPE_REG|REG_ONLY),
	DEFCONST(SP,     TYPE_REG|REG_ONLY),
	DEFCONST(IMM,    TYPE_REG|REG_IMM),
	DEFCONST(UNUSED, TYPE_REG|REG_ONLY), /* XXX only sljit.UNUSED */

	/* XXX SLJIT_MEM. */
	/* XXX Floating-point registers. */

	/* sljit_emit_op0. */
	DEFCONST(BREAKPOINT, TYPE_OP0),
	DEFCONST(NOP,        TYPE_OP0),
	DEFCONST(LUMUL,      TYPE_OP0),
	DEFCONST(LSMUL,      TYPE_OP0),
	DEFCONST(LUDIV,      TYPE_OP0),
	DEFCONST(ILUDIV,     TYPE_OP0),
	DEFCONST(LSDIV,      TYPE_OP0),
	DEFCONST(ILSDIV,     TYPE_OP0),

	/* sljit_emit_op1. */
	DEFCONST(MOV,      TYPE_OP1|OP1_RET),
	DEFCONST(MOV_UB,   TYPE_OP1|OP1_RET),
	DEFCONST(IMOV_UB,  TYPE_OP1|OP1_RET),
	DEFCONST(MOV_SB,   TYPE_OP1|OP1_RET),
	DEFCONST(IMOV_SB,  TYPE_OP1|OP1_RET),
	DEFCONST(MOV_UH,   TYPE_OP1|OP1_RET),
	DEFCONST(IMOV_UH,  TYPE_OP1|OP1_RET),
	DEFCONST(MOV_SH,   TYPE_OP1|OP1_RET),
	DEFCONST(IMOV_SH,  TYPE_OP1|OP1_RET),
	DEFCONST(MOV_UI,   TYPE_OP1|OP1_RET),
	DEFCONST(MOV_SI,   TYPE_OP1|OP1_RET),
	DEFCONST(IMOV,     TYPE_OP1|OP1_RET),
	DEFCONST(MOV_P,    TYPE_OP1|OP1_RET),
	DEFCONST(MOVU,     TYPE_OP1),
	DEFCONST(MOVU_UB,  TYPE_OP1),
	DEFCONST(IMOVU_UB, TYPE_OP1),
	DEFCONST(MOVU_SB,  TYPE_OP1),
	DEFCONST(IMOVU_SB, TYPE_OP1),
	DEFCONST(MOVU_UH,  TYPE_OP1),
	DEFCONST(IMOVU_UH, TYPE_OP1),
	DEFCONST(MOVU_SH,  TYPE_OP1),
	DEFCONST(IMOVU_SH, TYPE_OP1),
	DEFCONST(MOVU_UI,  TYPE_OP1),
	DEFCONST(MOVU_SI,  TYPE_OP1),
	DEFCONST(IMOVU,    TYPE_OP1),
	DEFCONST(MOVU_P,   TYPE_OP1),
	DEFCONST(NOT,      TYPE_OP1),
	DEFCONST(INOT,     TYPE_OP1),
	DEFCONST(NEG,      TYPE_OP1),
	DEFCONST(INEG,     TYPE_OP1),
	DEFCONST(CLZ,      TYPE_OP1),
	DEFCONST(ICLZ,     TYPE_OP1),

	/* sljit_emit_op2. */
	DEFCONST(ADD,   TYPE_OP2),
	DEFCONST(IADD,  TYPE_OP2),
	DEFCONST(ADDC,  TYPE_OP2),
	DEFCONST(IADDC, TYPE_OP2),
	DEFCONST(SUB,   TYPE_OP2),
	DEFCONST(ISUB,  TYPE_OP2),
	DEFCONST(SUBC,  TYPE_OP2),
	DEFCONST(ISUBC, TYPE_OP2),
	DEFCONST(MUL,   TYPE_OP2),
	DEFCONST(IMUL,  TYPE_OP2),
	DEFCONST(AND,   TYPE_OP2),
	DEFCONST(IAND,  TYPE_OP2),
	DEFCONST(OR,    TYPE_OP2),
	DEFCONST(IOR,   TYPE_OP2),
	DEFCONST(XOR,   TYPE_OP2),
	DEFCONST(IXOR,  TYPE_OP2),
	DEFCONST(SHL,   TYPE_OP2),
	DEFCONST(ISHL,  TYPE_OP2),
	DEFCONST(LSHR,  TYPE_OP2),
	DEFCONST(ILSHR, TYPE_OP2),
	DEFCONST(ASHR,  TYPE_OP2),
	DEFCONST(IASHR, TYPE_OP2),

	/* TYPE_CMP constants. */
	DEFCONST(EQUAL,               TYPE_CMP|CMP_JMP),
	DEFCONST(I_EQUAL,             TYPE_CMP|CMP_JMP),
	DEFCONST(ZERO,                TYPE_CMP|CMP_JMP),
	DEFCONST(I_ZERO,              TYPE_CMP|CMP_JMP),
	DEFCONST(NOT_EQUAL,           TYPE_CMP|CMP_JMP),
	DEFCONST(I_NOT_EQUAL,         TYPE_CMP|CMP_JMP),
	DEFCONST(NOT_ZERO,            TYPE_CMP|CMP_JMP),
	DEFCONST(I_NOT_ZERO,          TYPE_CMP|CMP_JMP),
	DEFCONST(LESS,                TYPE_CMP|CMP_JMP),
	DEFCONST(I_LESS,              TYPE_CMP|CMP_JMP),
	DEFCONST(GREATER_EQUAL,       TYPE_CMP|CMP_JMP),
	DEFCONST(I_GREATER_EQUAL,     TYPE_CMP|CMP_JMP),
	DEFCONST(GREATER,             TYPE_CMP|CMP_JMP),
	DEFCONST(I_GREATER,           TYPE_CMP|CMP_JMP),
	DEFCONST(LESS_EQUAL,          TYPE_CMP|CMP_JMP),
	DEFCONST(I_LESS_EQUAL,        TYPE_CMP|CMP_JMP),
	DEFCONST(SIG_LESS,            TYPE_CMP|CMP_JMP),
	DEFCONST(I_SIG_LESS,          TYPE_CMP|CMP_JMP),
	DEFCONST(SIG_GREATER_EQUAL,   TYPE_CMP|CMP_JMP),
	DEFCONST(I_SIG_GREATER_EQUAL, TYPE_CMP|CMP_JMP),
	DEFCONST(SIG_GREATER,         TYPE_CMP|CMP_JMP),
	DEFCONST(I_SIG_GREATER,       TYPE_CMP|CMP_JMP),
	DEFCONST(SIG_LESS_EQUAL,      TYPE_CMP|CMP_JMP),
	DEFCONST(I_SIG_LESS_EQUAL,    TYPE_CMP|CMP_JMP),
	DEFCONST(OVERFLOW,            TYPE_CMP),
	DEFCONST(I_OVERFLOW,          TYPE_CMP),
	DEFCONST(NOT_OVERFLOW,        TYPE_CMP),
	DEFCONST(I_NOT_OVERFLOW,      TYPE_CMP),
	DEFCONST(MUL_OVERFLOW,        TYPE_CMP),
	DEFCONST(I_MUL_OVERFLOW,      TYPE_CMP),
	DEFCONST(MUL_NOT_OVERFLOW,    TYPE_CMP),
	DEFCONST(I_MUL_NOT_OVERFLOW,  TYPE_CMP),
};

/* sljit_compiler userdata. */
struct luaSljitCompiler
{
	struct sljit_compiler *compiler;
};

/* sljit_jump userdata. */
struct luaSljitJump
{
	struct sljit_jump *jump;
};

/* sljit_label userdata. */
struct luaSljitLabel
{
	struct sljit_label *label;
};

/* sljit_const userdata. */
struct luaSljitConst
{
	struct sljit_const *const_;
};

/* Userdata for generated code. */
struct luaSljitCode
{
	void *code;
};

static void *
testudata(lua_State *L, int narg, const char *tname)
{
	void *udata;

#if LUA_VERSION_NUM >= 502
	udata = luaL_testudata(L, narg, tname);
#else
	udata = lua_touserdata(L, narg);
	if (udata != NULL && lua_getmetatable(L, narg)) {
		luaL_getmetatable(L, tname);
		if (!lua_rawequal(L, -1, -2))
			udata = NULL;
		lua_pop(L, 2);
	}
#endif
	return udata;
}

static int
badargtype(lua_State *L, const char *type)
{

	return luaL_error(L, "invalid %s", type != NULL ? type : ARG_METATABLE);
}

#if 0
/* luaL_typerror() was removed after 5.1. */
static int
typerror(lua_State *L, int narg, const char *tname)
{
	const char *msg;

	msg = lua_pushfstring(L, "%s expected, got %s",
	    tname, luaL_typename(L, narg));
	return luaL_argerror(L, narg, msg);
}
#endif

static void
getuservalue(lua_State *L, int index)
{

#if LUA_VERSION_NUM <= 501
	lua_getfenv(L, index);
#else
	lua_getuservalue(L, index);
#endif
}

static void
setuservalue(lua_State *L, int index)
{

#if LUA_VERSION_NUM <= 501
	lua_setfenv(L, index);
#else
	lua_setuservalue(L, index);
#endif
}

static void
pusharg(lua_State *L, const struct luaSljitArg *arg)
{
	struct luaSljitArg *udata;

	udata = (struct luaSljitArg *)
	    lua_newuserdata(L, sizeof(struct luaSljitArg));
	*udata = *arg;

	luaL_getmetatable(L, ARG_METATABLE);
	lua_setmetatable(L, -2);
}

static struct luaSljitArg *
checkarg(lua_State *L, int narg, const char *type, constant_flag_t flags)
{
	struct luaSljitArg *res;

	res = (struct luaSljitArg *)luaL_checkudata(L, narg, ARG_METATABLE);

	if (flags != 0 && (res->flags & flags) == 0)
		badargtype(L, type);

	return res;
}

static bool
parsearg(lua_State *L, const char *str, struct luaSljitArg *copyto)
{
	struct luaSljitArg tmp;
	const struct luaSljitArg *ud;
	const char *end;
	const char delim = '+';

	if (str == NULL)
		return false;

	lua_pushlightuserdata(L, &constants);
	lua_rawget(L, LUA_REGISTRYINDEX);

	tmp.flags = 0;
	tmp.argi = 0;
	tmp.argw = 0;

	end = strchr(str, delim);
	while (end != NULL) {
		lua_pushlstring(L, str, end - str);
		lua_rawget(L, -2);

		if (lua_isnil(L, -1)) {
			lua_pop(L, 2);
			return false;
		}

		/* Unchecked access is faster. */
		assert(checkarg(L, -1, NULL, 0));
		ud = (const struct luaSljitArg *)lua_touserdata(L, -1);

		tmp.flags |= ud->flags;
		tmp.argi |= ud->argi;
		lua_pop(L, 1);

		str = end + 1;
		end = strchr(str, delim);
	}

	lua_pushstring(L, str);
	lua_rawget(L, -2);

	/* Unchecked access is faster. */
	assert(checkarg(L, -1, NULL, 0));
	ud = (const struct luaSljitArg *)lua_touserdata(L, -1);

	tmp.flags |= ud->flags;
	tmp.argi |= ud->argi;

	lua_pop(L, 2);

	if (copyto != NULL)
		*copyto = tmp;
	else
		pusharg(L, &tmp);

	return true;
}

static struct luaSljitArg *
toarg(lua_State *L, int narg, const char *type, int flags,
    struct luaSljitArg *copyto)
{
	struct luaSljitArg *arg, tmp;

	assert(narg > 0);

	switch (lua_type(L, narg)) {
	case LUA_TUSERDATA:
		arg = checkarg(L, narg, type, flags);
		break;
	case LUA_TNUMBER:
		if (flags != 0 && (flags & REG_IMM) == 0)
			badargtype(L, type);
		arg = &tmp;
		tmp.flags = TYPE_REG|REG_IMM;
		tmp.argi = SLJIT_IMM;
		tmp.argw = lua_tointeger(L, narg);
		break;
	default:
		// XXX convert bignum values
		arg = &tmp;
		if (!parsearg(L, lua_tostring(L, narg), arg))
			luaL_argerror(L, narg, ERR_NOCONV(ARG_METATABLE));
	}

	if (copyto != NULL) {
		*copyto = *arg;
		return copyto;
	}

	return arg == &tmp ? NULL : arg;
}

static void
checkreg(lua_State *L, int narg, int flags, sljit_si *regi, sljit_sw *regw)
{
	struct luaSljitArg arg;

	toarg(L, narg, "register", flags, &arg);
	*regi = arg.argi;
	*regw = arg.argw;
}

/*
 * Extract an integer field k from argpack at index t.
 */
static lua_Integer
getiarg(lua_State *L, int t, const char *k, lua_Integer dflt)
{

	lua_pushstring(L, k);
	lua_rawget(L, t);

	if (lua_type(L, -1) == LUA_TNIL)
		return dflt;
	else
		return lua_tonumber(L, -1);
}


#if 0
/*
 * Test that n is an integer in [INT32_MIN, INT32_MAX] range.
 * lua_Number can be double, long double, int32_t or other signed
 * integer type with a greater width.
 */
static inline bool
is_int32(lua_Number n)
{

	return n >= INT32_MIN && n <= INT32_MAX && n == (lua_Integer)n;
}

/* XXX remove */
static sljit_sw
strtosw(const char *s, size_t slen, bool *err)
{
	long long ll;
	long l;
	char *se;
	const int base = 0; /* 8, 10, or 16 */

	errno = 0;

	if (sizeof(sljit_sw) <= sizeof(long)) {
		l = strtol(s, &se, base);
		if (slen == 0 || se != s + slen)
			*err = true;
		if ((l == LONG_MAX || l == LONG_MIN) && errno != 0)
			*err = true;
		else
			*err = (l != (sljit_sw)l);
		return l;
	} else {
		ll = strtoll(s, &se, base);
		if (slen == 0 || se != s + slen)
			*err = true;
		if ((ll == LLONG_MAX || ll == LLONG_MIN) && errno != 0)
			*err = true;
		else
			*err = (ll != (sljit_sw)ll);
		return ll;
	}
}
#endif

#if 0
static sljit_sw
usertypetosw(lua_State *L, int narg)
{
	const char *s;
	size_t i, l;
	sljit_uw u;

	assert(narg > 0);

	lua_getfield(L, narg, "tobin");
	lua_pushvalue(L, narg);
	lua_call(L, 1, 1);

	s = lua_tolstring(L, -1, &l);

	if (s == NULL || l > sizeof(sljit_sw))
		luaL_argerror(L, narg, ERR_NOCONV("sljit_sw"));

	u = 0;
	for (i = l; i > 0; i--)
		u = (unsigned char)s[i-1] + 256 * u;

	lua_pop(L, 1);

	lua_getfield(L, narg, "isneg");
	lua_pushvalue(L, narg);
	lua_call(L, 1, 1);

	if (lua_toboolean(L, -1))
		u = -u;

	lua_pop(L, 1);

	return u;
}
#endif

static sljit_sw
tosw(lua_State *L, int narg)
{
#if LUA_VERSION_NUM >= 503
	lua_Integer i;
	int isnum;
#else
	lua_Number d;
#endif
	assert(narg > 0);

#if LUA_VERSION_NUM >= 503
	i = lua_tointegerx(L, narg, &isnum);

/* XXX
	if (!isnum) {
		switch (lua_type(L, narg)) {
		case LUA_TTABLE:
		case LUA_TUSERDATA:
			return usertypetosw(L, narg, narg);
		}
	}
*/

	if (!isnum)
		luaL_argerror(L, narg, ERR_NOCONV("sljit_sw"));

	return i;
#else
	d = lua_tonumber(L, narg);
	if (d != 0 && d == (sljit_sw)d)
		return d;

	switch (lua_type(L, narg)) {
	case LUA_TNUMBER:
		if (d != 0)
			luaL_argerror(L, narg, ERR_NOCONV("sljit_sw"));
		return 0;
	case LUA_TSTRING:
		/* XXX */
		return 0;
/* XXX
	case LUA_TTABLE:
	case LUA_TUSERDATA:
		return usertypetosw(L, narg, narg);
*/
	default:
		luaL_argerror(L, narg, ERR_NOCONV("sljit_sw"));
	}

	/* NOTREACHED */
	return 0;
#endif
}

static int
l_imm(lua_State *L)
{
	struct luaSljitArg res;

	res.flags = 0; // XXX
	res.argi = SLJIT_IMM;
	res.argw = tosw(L, 1);

	pusharg(L, &res);
	return 1;
}

static int
l_mem0(lua_State *L)
{
	struct luaSljitArg res;

	res.flags = 0; // XXX
	res.argi = SLJIT_MEM0();
	res.argw = tosw(L, 1);

	pusharg(L, &res);
	return 1;
}

static int
l_mem1(lua_State *L)
{
	struct luaSljitArg arg;

	toarg(L, 1, "register", REG_ONLY, &arg);
	arg.argi = SLJIT_MEM1(arg.argi);
	arg.argw = tosw(L, 2);

	pusharg(L, &arg);
	return 1;
}

static int
l_mem2(lua_State *L)
{
	struct luaSljitArg arg1, arg2;

	toarg(L, 1, "register", REG_ONLY, &arg1);
	toarg(L, 2, "register", REG_ONLY, &arg2);
	arg1.argi = SLJIT_MEM2(arg1.argi, arg2.argi);
	arg1.argw = tosw(L, 3);

	pusharg(L, &arg1);
	return 1;
}

static int
l_unaligned(lua_State *L)
{

#if defined(SLJIT_UNALIGNED) && SLJIT_UNALIGNED
	lua_pushboolean(L, true);
#else
	lua_pushboolean(L, true);
#endif

	return 1;
}

static int
l_is_fpu_available(lua_State *L)
{

	lua_pushboolean(L, sljit_is_fpu_available());

	return 1;
}

static int
l_word_width(lua_State *L)
{

#if defined(SLJIT_32BIT_ARCHITECTURE) && SLJIT_32BIT_ARCHITECTURE
	lua_pushinteger(L, 4);
#elif defined(SLJIT_64BIT_ARCHITECTURE) && SLJIT_64BIT_ARCHITECTURE
	lua_pushinteger(L, 8);
#else
	return luaL_error(L, "sljit is misconfigured");
#endif
	return 1;
}

static int
l_create_compiler(lua_State *L)
{
	struct luaSljitCompiler *udata;

	udata = (struct luaSljitCompiler *)
	    lua_newuserdata(L, sizeof(struct luaSljitCompiler));
	udata->compiler = NULL;

	luaL_getmetatable(L, COMP_METATABLE);
	lua_setmetatable(L, -2);

	udata->compiler = sljit_create_compiler(NULL);

	if (udata->compiler == NULL)
		return luaL_error(L, "sljit.create_compiler() failed");

	return 1;
}

static int
gc_compiler(lua_State *L)
{
	struct luaSljitCompiler *udata;
	struct sljit_compiler *compiler;

	udata = (struct luaSljitCompiler *)
	    luaL_checkudata(L, 1, COMP_METATABLE);
	compiler = udata->compiler;

	udata->compiler = NULL;
	if (compiler != NULL)
		sljit_free_compiler(compiler);

	lua_pushnil(L);
	lua_setmetatable(L, 1);

	return 0;
}

static int
gc_code(lua_State *L)
{
	struct luaSljitCode *udata;
	void *code;

	udata = (struct luaSljitCode *)
	    luaL_checkudata(L, 1, CODE_METATABLE);
	code = udata->code;

	udata->code = NULL;
	if (code != NULL)
		sljit_free_code(code);

	lua_pushnil(L);
	lua_setmetatable(L, 1);

	return 0;
}

static int
compiler_error(lua_State *L, const char *fname, int status)
{

	/* XXX convert status to string. */
	return luaL_error(L, "%s failed with %d", fname, status);
}

static struct luaSljitCompiler *
checkcompiler(lua_State *L, int narg)
{
	struct luaSljitCompiler *comp;

	comp = (struct luaSljitCompiler *)
	    luaL_checkudata(L, narg, COMP_METATABLE);

	if (comp->compiler == NULL)
		luaL_error(L, COMP_METATABLE " object is dead");

	return comp;
}

static struct luaSljitJump *
checkjump(lua_State *L, int narg)
{
	struct luaSljitJump *jump;

	jump = (struct luaSljitJump *)
	    luaL_checkudata(L, narg, JUMP_METATABLE);

	if (jump->jump == NULL)
		luaL_error(L, JUMP_METATABLE " object is dead");

	return jump;
}

static struct luaSljitLabel *
checklabel(lua_State *L, int narg)
{
	struct luaSljitLabel *label;

	label = (struct luaSljitLabel *)
	    luaL_checkudata(L, narg, LABEL_METATABLE);

	if (label->label == NULL)
		luaL_error(L, LABEL_METATABLE " object is dead");

	return label;
}

#if 0
static struct luaSljitConst *
checkconst(lua_State *L, int narg)
{
	struct luaSljitConst *const_;

	const_ = (struct luaSljitConst *)
	    luaL_checkudata(L, narg, CONST_METATABLE);

	if (const_->const_ == NULL)
		luaL_error(L, CONST_METATABLE " object is dead");

	return const_;
}
#endif

static bool
binop_option_arguments(lua_State *L, struct luaSljitArg **first,
    struct luaSljitArg **second, struct luaSljitArg *tmp)
{
	struct luaSljitArg *args[2] = { NULL, NULL };
	int i;

	for (i = 1; i <= 2; i++) {
		switch (lua_type(L, i)) {
		case LUA_TUSERDATA:
			args[i - 1] = checkarg(L, i, NULL, 0);
			break;
		default:
			if (!parsearg(L, lua_tostring(L, i), tmp))
				return luaL_argerror(L, i, ERR_NOCONV(ARG_METATABLE));
			break;
		}
	}

	if (args[0] == NULL && args[1] == NULL)
		return false;

	*first = args[0];
	*second = args[1];

	return true;
}

static int
l_arg_add(lua_State *L)
{
	struct luaSljitArg tmp, res, *first, *second;

	if (!binop_option_arguments(L, &first, &second, &tmp))
		return luaL_error(L, "no userdata");

	res = *first;
	res.flags |= second->flags;
	res.argi |= second->argi;
	res.argw = 0; // XXX

	pusharg(L, &res);
	return 1;
}

static int
l_arg_sub(lua_State *L)
{
	struct luaSljitArg tmp, res, *first, *second;

	if (!binop_option_arguments(L, &first, &second, &tmp))
		return luaL_error(L, "no userdata");

	res = *first;
	res.flags &= ~second->flags;
	res.argi &= ~second->argi;
	res.argw = 0; // XXX

	pusharg(L, &res);
	return 1;
}

static int
l_arg_tostr(lua_State *L)
{
	struct luaSljitArg *arg;

	arg = checkarg(L, 1, NULL, 0);

	lua_pushfstring(L, "<sljit.C option: %d>", (int)arg->argi); // XXX
	return 1;
}

static int
l_verbose(lua_State *L)
{
#if (defined SLJIT_VERBOSE && SLJIT_VERBOSE)
	struct luaSljitCompiler *comp;
#if LUA_VERSION_NUM >= 502
	luaL_Stream *stream;
#endif
	FILE *file = NULL;

	comp = checkcompiler(L, 1);

	if (lua_toboolean(L, 2)) {
#if LUA_VERSION_NUM >= 502
		stream = (luaL_Stream *)luaL_checkudata(L, 2, LUA_FILEHANDLE);
		file = stream->f;
#else
		file = *((FILE **)luaL_checkudata(L, 2, LUA_FILEHANDLE));
#endif
	}

	if (file != NULL)
		lua_pushvalue(L, 2);
	else
		lua_pushnil(L);

	sljit_compiler_verbose(comp->compiler, file);
	setuservalue(L, 1); /* Don't gc FILE udata too early. */
#endif
	return 0;
}

static int
l_emit_enter(lua_State *L)
{
	struct luaSljitCompiler *comp;
	sljit_si options, args, scratches, saveds;
	sljit_si fscratches, fsaveds, local_size;
	int status;

	comp = checkcompiler(L, 1);

	if (lua_type(L, 2) != LUA_TTABLE) {
		options    = luaL_checkinteger(L, 2);
		args       = luaL_checkinteger(L, 3);
		scratches  = luaL_checkinteger(L, 4);
		saveds     = luaL_checkinteger(L, 5);
		fscratches = luaL_checkinteger(L, 6);
		fsaveds    = luaL_checkinteger(L, 7);
		local_size = luaL_checkinteger(L, 8);
	} else {
		options    = getiarg(L, 2, "options", 0);
		args       = getiarg(L, 2, "args", 0);
		scratches  = getiarg(L, 2, "scratches", 0);
		saveds     = getiarg(L, 2, "saveds", 0);
		fscratches = getiarg(L, 2, "fscratches", 0);
		fsaveds    = getiarg(L, 2, "fsaveds", 0);
		local_size = getiarg(L, 2, "local_size", 0);
	}

	status = sljit_emit_enter(comp->compiler, options, args,
	    scratches, saveds, fscratches, fsaveds, local_size);
	if (status != SLJIT_SUCCESS)
		return compiler_error(L, "sljit_emit_enter", status);

	lua_pushvalue(L, 1);
	return 1;
}

static int
l_emit_op0(lua_State *L)
{
	struct luaSljitCompiler *comp;
	struct luaSljitArg op;
	int status;

	comp = checkcompiler(L, 1);
	toarg(L, 2, "opcode", 0, &op);

	status = sljit_emit_op0(comp->compiler, op.argi);
	if (status != SLJIT_SUCCESS)
		return compiler_error(L, "sljit_emit_op0", status);

	lua_pushvalue(L, 1);
	return 1;
}

static int
l_emit_op1(lua_State *L)
{
	struct luaSljitCompiler *comp;
	struct luaSljitArg op;
	sljit_sw dstw, srcw;
	sljit_si dst, src;
	int status;

	comp = checkcompiler(L, 1);
	toarg(L, 2, "opcode", 0, &op);
	checkreg(L, 3, REG_ONLY, &dst, &dstw);
	checkreg(L, 4, REG_IMM,  &src, &srcw);

	status = sljit_emit_op1(comp->compiler, op.argi, dst, dstw, src, srcw);
	if (status != SLJIT_SUCCESS)
		return compiler_error(L, "sljit_emit_op1", status);

	lua_pushvalue(L, 1);
	return 1;
}

static int
l_emit_op2(lua_State *L)
{
	struct luaSljitCompiler *comp;
	struct luaSljitArg op;
	sljit_sw dstw, src1w, src2w;
	sljit_si dst, src1, src2;
	int status;

	comp = checkcompiler(L, 1);
	toarg(L, 2, "opcode", 0, &op);
	checkreg(L, 3, REG_ONLY, &dst, &dstw);
	checkreg(L, 4, REG_IMM,  &src1, &src1w);
	checkreg(L, 5, REG_IMM,  &src2, &src2w);

	status = sljit_emit_op2(comp->compiler, op.argi,
	    dst, dstw, src1, src1w, src2, src2w);
	if (status != SLJIT_SUCCESS)
		return compiler_error(L, "sljit_emit_op2", status);

	lua_pushvalue(L, 1);
	return 1;
}

static int
l_emit_return(lua_State *L)
{
	struct luaSljitCompiler *comp;
	struct luaSljitArg op;
	sljit_sw srcw;
	sljit_si src;
	int status;

	comp = checkcompiler(L, 1);
	/* XXX SLJIT_UNUSED opcode */
	toarg(L, 2, "opcode", OP1_RET, &op);
	checkreg(L, 3, REG_IMM, &src, &srcw);

	status = sljit_emit_return(comp->compiler, op.argi, src, srcw);
	if (status != SLJIT_SUCCESS)
		return compiler_error(L, "sljit_emit_return", status);

	lua_pushvalue(L, 1);
	return 1;
}

static int
l_emit_fast_enter(lua_State *L)
{
	struct luaSljitCompiler *comp;
	sljit_sw dstw;
	sljit_si dst;
	int status;

	comp = checkcompiler(L, 1);
	checkreg(L, 2, REG_ONLY, &dst, &dstw);

	status = sljit_emit_fast_enter(comp->compiler, dst, dstw);
	if (status != SLJIT_SUCCESS)
		return compiler_error(L, "sljit_emit_fast_enter", status);

	lua_pushvalue(L, 1);
	return 1;
}

static int
l_emit_fast_return(lua_State *L)
{
	struct luaSljitCompiler *comp;
	sljit_sw srcw;
	sljit_si src;
	int status;

	comp = checkcompiler(L, 1);
	checkreg(L, 2, REG_ONLY, &src, &srcw);

	status = sljit_emit_fast_return(comp->compiler, src, srcw);
	if (status != SLJIT_SUCCESS)
		return compiler_error(L, "sljit_emit_fast_return", status);

	lua_pushvalue(L, 1);
	return 1;
}

static int
l_get_local_base(lua_State *L)
{
	struct luaSljitCompiler *comp;
	sljit_sw dstw, offset;
	sljit_si dst;
	int status;

	comp = checkcompiler(L, 1);
	checkreg(L, 2, REG_ONLY, &dst, &dstw);
	offset = tosw(L, 3);

	status = sljit_get_local_base(comp->compiler, dst, dstw, offset);
	if (status != SLJIT_SUCCESS)
		return compiler_error(L, "sljit_get_local_base", status);

	return 0;
}

static int
l_get_compiler_error(lua_State *L)
{
	struct luaSljitCompiler *comp;
	int status;

	comp = checkcompiler(L, 1);

	status = sljit_get_compiler_error(comp->compiler);

	lua_pushinteger(L, status); /* XXX pushstring */

	return 1;
}

static int
l_get_generated_code_size(lua_State *L)
{
	struct luaSljitCompiler *comp;
	sljit_uw sz;

	comp = checkcompiler(L, 1);

	sz = sljit_get_generated_code_size(comp->compiler);

	lua_pushinteger(L, sz);

	return 1;
}

static int
l_emit_jump(lua_State *L)
{
	struct luaSljitCompiler *comp;
	struct luaSljitArg type;
	struct luaSljitJump *udata;

	comp = checkcompiler(L, 1);
	toarg(L, 2, "jump", 0, &type);

	udata = (struct luaSljitJump *)
	    lua_newuserdata(L, sizeof(struct luaSljitJump));
	udata->jump = NULL;

	luaL_getmetatable(L, JUMP_METATABLE);
	lua_setmetatable(L, -2);

	/*
	 * Make sure that the compiler object (argument 1) isn't
	 * garbage collected before the jump object (at the top).
	 */
	lua_createtable(L, 1, 0);
	lua_pushvalue(L, 1);
	lua_rawseti(L, -2, COMPILER_UVAL_INDEX);
	setuservalue(L, -2);

	udata->jump = sljit_emit_jump(comp->compiler, type.argi);
	if (udata->jump == NULL)
		return luaL_error(L, "sljit.emit_jump() failed");

	return 1;
}

static int
l_emit_cmp(lua_State *L)
{
	struct luaSljitCompiler *comp;
	struct luaSljitJump *udata;
	struct luaSljitArg type;
	sljit_sw src1w, src2w;
	sljit_si src1, src2;

	comp = checkcompiler(L, 1);
	toarg(L, 2, "comparison", CMP_JMP, &type);
	checkreg(L, 3, REG_IMM, &src1, &src1w);
	checkreg(L, 4, REG_IMM, &src2, &src2w);

	udata = (struct luaSljitJump *)
	    lua_newuserdata(L, sizeof(struct luaSljitJump));
	udata->jump = NULL;

	luaL_getmetatable(L, JUMP_METATABLE);
	lua_setmetatable(L, -2);

	/*
	 * Make sure that the compiler object (argument 1) isn't
	 * garbage collected before the jump object (at the top).
	 */
	lua_createtable(L, 1, 0);
	lua_pushvalue(L, 1);
	lua_rawseti(L, -2, COMPILER_UVAL_INDEX);
	setuservalue(L, -2);

	udata->jump = sljit_emit_cmp(comp->compiler,
	    type.argi, src1, src1w, src2, src2w);
	if (udata->jump == NULL)
		return luaL_error(L, "sljit.emit_cmp() failed");

	return 1;
}

static int
l_emit_const(lua_State *L)
{
	struct luaSljitCompiler *comp;
	struct luaSljitConst *udata;
	sljit_sw initval, dstw;
	sljit_si dst;

	comp = checkcompiler(L, 1);
	checkreg(L, 2, REG_ONLY, &dst, &dstw);
	initval = tosw(L, 3);

	udata = (struct luaSljitConst *)
	    lua_newuserdata(L, sizeof(struct luaSljitConst));
	udata->const_ = NULL;

	luaL_getmetatable(L, CONST_METATABLE);
	lua_setmetatable(L, -2);

	/*
	 * Make sure that the compiler object (argument 1) isn't
	 * garbage collected before the const_ object (at the top).
	 */
	lua_createtable(L, 1, 0);
	lua_pushvalue(L, 1);
	lua_rawseti(L, -2, COMPILER_UVAL_INDEX);
	setuservalue(L, -2);

	udata->const_ = sljit_emit_const(comp->compiler, dst, dstw, initval);
	if (udata->const_ == NULL)
		return luaL_error(L, "sljit.emit_const() failed");

	return 1;
}

static int
l_emit_label(lua_State *L)
{
	struct luaSljitCompiler *comp;
	struct luaSljitLabel *udata;

	comp = checkcompiler(L, 1);

	udata = (struct luaSljitLabel *)
	    lua_newuserdata(L, sizeof(struct luaSljitLabel));
	udata->label = NULL;

	luaL_getmetatable(L, LABEL_METATABLE);
	lua_setmetatable(L, -2);

	/*
	 * Make sure that the compiler object (argument 1) isn't
	 * garbage collected before the label object (at the top).
	 */
	lua_createtable(L, 1, 0);
	lua_pushvalue(L, 1);
	lua_rawseti(L, -2, COMPILER_UVAL_INDEX);
	setuservalue(L, -2);

	udata->label = sljit_emit_label(comp->compiler);
	if (udata->label == NULL)
		return luaL_error(L, "sljit.emit_label() failed");

	return 1;
}

static int
l_set_label(lua_State *L)
{
	struct luaSljitJump *jump;
	struct luaSljitLabel *label;

	jump = checkjump(L, 1);
	label = checklabel(L, 2);

	sljit_set_label(jump->jump, label->label);

	getuservalue(L, 1);
	lua_rawgeti(L, -1, COMPILER_UVAL_INDEX);

	return 1;
}

static int
l_generate_code(lua_State *L)
{
	struct luaSljitCompiler *comp;
	struct luaSljitCode *udata;

	comp = checkcompiler(L, 1);

	udata = (struct luaSljitCode *)
	    lua_newuserdata(L, sizeof(struct luaSljitCode));
	udata->code = NULL;

	luaL_getmetatable(L, CODE_METATABLE);
	lua_setmetatable(L, -2);

	udata->code = sljit_generate_code(comp->compiler);
	if (udata->code == NULL)
		return luaL_error(L, "sljit.generate_code() failed");

	return 1;
}

static luaL_Reg comp_metafunctions[] = {
	{ "__gc", gc_compiler },
	{ NULL, NULL }
};

static luaL_Reg code_metafunctions[] = {
	{ "__gc", gc_code },
	{ NULL, NULL }
};

static luaL_Reg comp_methods[] = {
	{ "emit_cmp",                l_emit_cmp                },
	{ "emit_const",              l_emit_const              },
	{ "emit_enter",              l_emit_enter              },
	{ "emit_fast_enter",         l_emit_fast_enter         },
	{ "emit_fast_return",        l_emit_fast_return        },
	{ "emit_jump",               l_emit_jump               },
	{ "emit_label",              l_emit_label              },
	{ "emit_op0",                l_emit_op0                },
	{ "emit_op1",                l_emit_op1                },
	{ "emit_op2",                l_emit_op2                },
	{ "emit_return",             l_emit_return             },
	{ "generate_code",           l_generate_code           },
	{ "get_compiler_error",      l_get_compiler_error      },
	{ "get_generated_code_size", l_get_generated_code_size },
	{ "get_local_base",          l_get_local_base          },
	{ "verbose",                 l_verbose                 },
	{ NULL, NULL }
};

static luaL_Reg arg_metafunctions[] = {
	{ "__add",      l_arg_add   },
	{ "__sub",      l_arg_sub   },
	{ "__tostring", l_arg_tostr },
	{ NULL, NULL }
};

static luaL_Reg code_methods[] = {
	{ NULL, NULL }
};

static luaL_Reg const_methods[] = {
	/* XXX sljit_get_const_addr() */
	{ NULL, NULL }
};

static luaL_Reg jump_methods[] = {
	/* XXX sljit_get_jump_addr() */
	{ "set_label", l_set_label },
	{ NULL, NULL }
};

static luaL_Reg label_methods[] = {
	/* XXX sljit_get_label_addr() */
	{ NULL, NULL }
};

static luaL_Reg sljit_functions[] = {
	{ "word_width",       l_word_width       },
	{ "create_compiler",  l_create_compiler  },
	{ "is_fpu_available", l_is_fpu_available },
	{ "imm",              l_imm              },
	{ "mem0",             l_mem0             },
	{ "mem1",             l_mem1             },
	{ "mem2",             l_mem2             },
	{ "unaligned",        l_unaligned        },
	{ NULL, NULL }
};

static int
register_udata(lua_State *L, int arg, const char *tname,
    const luaL_Reg *metafunctions, const luaL_Reg *methods)
{

	/* Copy sljit module to the top. */
	if (arg != -1)
		lua_pushvalue(L, arg);

	/* Register methods as module's functions, e.g. sljit.emit_return(). */
	if (methods != NULL) {
#if LUA_VERSION_NUM <= 501
		luaL_register(L, NULL, methods);
#else
		luaL_setfuncs(L, methods, 0);
#endif
	}

	luaL_newmetatable(L, tname);

	if (metafunctions != NULL) {
#if LUA_VERSION_NUM <= 501
		luaL_register(L, NULL, metafunctions);
#else
		luaL_setfuncs(L, metafunctions, 0);
#endif
	}

	/* Register methods, e.g. compiler:emit_return(). */
	if (methods != NULL) {
		lua_pushstring(L, "__index");
		lua_newtable(L);
#if LUA_VERSION_NUM <= 501
		luaL_register(L, NULL, methods);
#else
		luaL_setfuncs(L, methods, 0);
#endif
		lua_rawset(L, -3);
	}

	lua_pop(L, arg != -1 ? 2 : 1);

	return 0;
}

static void
register_constants(lua_State *L, int arg)
{
	size_t i, nconstants = sizeof(constants) / sizeof(constants[0]);
	int j, ntables = 3; /* sljit.C, sljit and a table in the registry. */

	/* Copy sljit module to the top. */
	if (arg != -1)
		lua_pushvalue(L, arg);

	/* One copy is for the sljit.C namespace. */
	lua_pushstring(L, "C");
	lua_createtable(L, 0, nconstants);

	/* Second copy is for the registry. */
	lua_pushlightuserdata(L, &constants);
	lua_createtable(L, 0, nconstants);

	for (i = 0; i < nconstants; i++) {
		lua_pushstring(L, constants[i].name); /* Push the key. */

		/* Push the value. */
		if ((constants[i].arg.flags & TYPE_MASK) == TYPE_NOTUD) {
			lua_pushinteger(L, constants[i].arg.argi);
		} else {
			pusharg(L, &constants[i].arg);
		}

		/* Copy the key and the value (ntables - 1) times. */
		for (j = 0; j < 2 * (ntables - 1); j++)
			lua_pushvalue(L, -2);

		for (j = 0; j < ntables; j++)
			lua_rawset(L, -1 - 2 * ntables);
	}

	lua_rawset(L, LUA_REGISTRYINDEX);
	lua_rawset(L, -3);

	if (arg != -1)
		lua_pop(L, 1);
}

int
luaopen_sljit(lua_State *L)
{

#if LUA_VERSION_NUM <= 501
	luaL_register(L, "sljit", sljit_functions);
#else
	luaL_newlib(L, sljit_functions);
#endif

	register_udata(L, -1, ARG_METATABLE, arg_metafunctions, NULL);
	register_constants(L, -1);

	register_udata(L, -1, CONST_METATABLE, NULL, const_methods);
	register_udata(L, -1, JUMP_METATABLE,  NULL, jump_methods);
	register_udata(L, -1, LABEL_METATABLE, NULL, label_methods);

	register_udata(L, -1, CODE_METATABLE, code_metafunctions, code_methods);
	register_udata(L, -1, COMP_METATABLE, comp_metafunctions, comp_methods);

	return 1;
}

struct sljit_compiler *
luaSljit_tocompiler(lua_State *L, int narg)
{
	struct luaSljitCompiler *ud;

	ud = (struct luaSljitCompiler *)testudata(L, narg, COMP_METATABLE);
	return (ud != NULL) ? ud->compiler : NULL;
}

void *
luaSljit_tocode(lua_State *L, int narg)
{
	struct luaSljitCode *ud;

	ud = (struct luaSljitCode *)testudata(L, narg, CODE_METATABLE);
	return (ud != NULL) ? ud->code : NULL;
}

struct sljit_const *
luaSljit_toconst(lua_State *L, int narg)
{
	struct luaSljitConst *ud;

	ud = (struct luaSljitConst *)testudata(L, narg, CONST_METATABLE);
	return (ud != NULL) ? ud->const_ : NULL;
}

struct sljit_jump *
luaSljit_tojump(lua_State *L, int narg)
{
	struct luaSljitJump *ud;

	ud = (struct luaSljitJump *)testudata(L, narg, JUMP_METATABLE);
	return (ud != NULL) ? ud->jump : NULL;
}

struct sljit_label *
luaSljit_tolabel(lua_State *L, int narg)
{
	struct luaSljitLabel *ud;

	ud = (struct luaSljitLabel *)testudata(L, narg, LABEL_METATABLE);
	return (ud != NULL) ? ud->label : NULL;
}

struct sljit_compiler *
luaSljit_get_compiler(lua_State *L, int narg)
{
	struct sljit_compiler *res;
	int npop = 0;

	if (testudata(L, narg, JUMP_METATABLE) != NULL ||
	    testudata(L, narg, LABEL_METATABLE) != NULL ||
	    testudata(L, narg, CONST_METATABLE) != NULL) {
		getuservalue(L, narg);
		lua_rawgeti(L, -1, COMPILER_UVAL_INDEX);
		narg = -1;
		npop = 2;
	}

	res = luaSljit_tocompiler(L, narg);
	if (npop > 0)
		lua_pop(L, npop);
	return res;
}
