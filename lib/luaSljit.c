/*-
 * Copyright (c) 2013-2014 Alexander Nasonov.
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

/* Indices in uservalue table */
#define COMPILER_UVAL_INDEX 1

/* Errors. */
#define ERR_NOCONV(type) "conversion to " type " failed"

#define DEFOPT(name, flags) { #name, SLJIT_##name, flags }
#define DEFOPT_END { NULL, 0, 0 }
struct option
{
	const char *name;
	sljit_si value;
	int flags;
};

/* XXX PREF_SHIFT_REG R2, RETURN_REG R0 */
#define REG_IMM  0
#define REG_ONLY 1
static const struct option regs[] = {
	DEFOPT(UNUSED, REG_ONLY), /* XXX only sljit.UNUSED */
	DEFOPT(S0,     REG_ONLY),
	DEFOPT(S1,     REG_ONLY),
	DEFOPT(S2,     REG_ONLY),
	DEFOPT(S3,     REG_ONLY),
	DEFOPT(S4,     REG_ONLY),
	DEFOPT(S5,     REG_ONLY),
	DEFOPT(S6,     REG_ONLY),
	DEFOPT(S7,     REG_ONLY),
	DEFOPT(S8,     REG_ONLY),
	DEFOPT(S9,     REG_ONLY),
	DEFOPT(R0,     REG_ONLY),
	DEFOPT(R1,     REG_ONLY),
	DEFOPT(R2,     REG_ONLY),
	DEFOPT(R3,     REG_ONLY),
	DEFOPT(R4,     REG_ONLY),
	DEFOPT(R5,     REG_ONLY),
	DEFOPT(R6,     REG_ONLY),
	DEFOPT(R7,     REG_ONLY),
	DEFOPT(R8,     REG_ONLY),
	DEFOPT(R9,     REG_ONLY),
	DEFOPT(SP,     REG_ONLY),
	DEFOPT(IMM,    REG_IMM),
	DEFOPT_END
};

/* XXX fregs for FPU registers. */

static const struct option ops0[] = {
	DEFOPT(BREAKPOINT, 0),
	DEFOPT(NOP, 0),
	DEFOPT(LUMUL, 0),
	DEFOPT(LSMUL, 0),
	DEFOPT(LUDIV, 0),
	DEFOPT(ILUDIV, 0),
	DEFOPT(LSDIV, 0),
	DEFOPT(ILSDIV, 0),
	DEFOPT_END
};

#define OP1_RET 1 /* can be passed to sljit_emit_return() */
static const struct option ops1[] = {
	DEFOPT(MOV, OP1_RET),
	DEFOPT(MOV_UB, OP1_RET),
	DEFOPT(IMOV_UB, OP1_RET),
	DEFOPT(MOV_SB, OP1_RET),
	DEFOPT(IMOV_SB, OP1_RET),
	DEFOPT(MOV_UH, OP1_RET),
	DEFOPT(IMOV_UH, OP1_RET),
	DEFOPT(MOV_SH, OP1_RET),
	DEFOPT(IMOV_SH, OP1_RET),
	DEFOPT(MOV_UI, OP1_RET),
	DEFOPT(MOV_SI, OP1_RET),
	DEFOPT(IMOV, OP1_RET),
	DEFOPT(MOV_P, OP1_RET),
	DEFOPT(MOVU, 0),
	DEFOPT(MOVU_UB, 0),
	DEFOPT(IMOVU_UB, 0),
	DEFOPT(MOVU_SB, 0),
	DEFOPT(IMOVU_SB, 0),
	DEFOPT(MOVU_UH, 0),
	DEFOPT(IMOVU_UH, 0),
	DEFOPT(MOVU_SH, 0),
	DEFOPT(IMOVU_SH, 0),
	DEFOPT(MOVU_UI, 0),
	DEFOPT(MOVU_SI, 0),
	DEFOPT(IMOVU, 0),
	DEFOPT(MOVU_P, 0),
	DEFOPT(NOT, 0),
	DEFOPT(INOT, 0),
	DEFOPT(NEG, 0),
	DEFOPT(INEG, 0),
	DEFOPT(CLZ, 0),
	DEFOPT(ICLZ, 0),
	DEFOPT_END
};

static const struct option ops2[] = {
	DEFOPT(ADD, 0),
	DEFOPT(IADD, 0),
	DEFOPT(ADDC, 0),
	DEFOPT(IADDC, 0),
	DEFOPT(SUB, 0),
	DEFOPT(ISUB, 0),
	DEFOPT(SUBC, 0),
	DEFOPT(ISUBC, 0),
	DEFOPT(MUL, 0),
	DEFOPT(IMUL, 0),
	DEFOPT(AND, 0),
	DEFOPT(IAND, 0),
	DEFOPT(OR, 0),
	DEFOPT(IOR, 0),
	DEFOPT(XOR, 0),
	DEFOPT(IXOR, 0),
	DEFOPT(SHL, 0),
	DEFOPT(ISHL, 0),
	DEFOPT(LSHR, 0),
	DEFOPT(ILSHR, 0),
	DEFOPT(ASHR, 0),
	DEFOPT(IASHR, 0),
	DEFOPT_END
};

/* XXX
 * jump "can be combined (or'ed) with SLJIT_REWRITABLE_JUMP"
 * cmp  "can be combined (or'ed) with SLJIT_REWRITABLE_JUMP or SLJIT_INT_OP"
 */
#define JUMP_CMP 1
static const struct option jumptypes[] = {
	DEFOPT(EQUAL, JUMP_CMP),
	DEFOPT(I_EQUAL, JUMP_CMP),
	DEFOPT(ZERO, JUMP_CMP),
	DEFOPT(I_ZERO, JUMP_CMP),
	DEFOPT(NOT_EQUAL, JUMP_CMP),
	DEFOPT(I_NOT_EQUAL, JUMP_CMP),
	DEFOPT(NOT_ZERO, JUMP_CMP),
	DEFOPT(I_NOT_ZERO, JUMP_CMP),
	DEFOPT(LESS, JUMP_CMP),
	DEFOPT(I_LESS, JUMP_CMP),
	DEFOPT(GREATER_EQUAL, JUMP_CMP),
	DEFOPT(I_GREATER_EQUAL, JUMP_CMP),
	DEFOPT(GREATER, JUMP_CMP),
	DEFOPT(I_GREATER, JUMP_CMP),
	DEFOPT(LESS_EQUAL, JUMP_CMP),
	DEFOPT(I_LESS_EQUAL, JUMP_CMP),
	DEFOPT(SIG_LESS, JUMP_CMP),
	DEFOPT(I_SIG_LESS, JUMP_CMP),
	DEFOPT(SIG_GREATER_EQUAL, JUMP_CMP),
	DEFOPT(I_SIG_GREATER_EQUAL, JUMP_CMP),
	DEFOPT(SIG_GREATER, JUMP_CMP),
	DEFOPT(I_SIG_GREATER, JUMP_CMP),
	DEFOPT(SIG_LESS_EQUAL, JUMP_CMP),
	DEFOPT(I_SIG_LESS_EQUAL, JUMP_CMP),
	DEFOPT(OVERFLOW, 0),
	DEFOPT(I_OVERFLOW, 0),
	DEFOPT(NOT_OVERFLOW, 0),
	DEFOPT(I_NOT_OVERFLOW, 0),
	DEFOPT(MUL_OVERFLOW, 0),
	DEFOPT(I_MUL_OVERFLOW, 0),
	DEFOPT(MUL_NOT_OVERFLOW, 0),
	DEFOPT(I_MUL_NOT_OVERFLOW, 0),
	DEFOPT_END
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

static sljit_si
checkoption(lua_State *L, int index, int narg,
    const struct option *options, const char *name, int flags)
{
	const char *str, *err;
	size_t i;
	sljit_si num;
	int isnum;

#if LUA_VERSION_NUM >= 502
	num = lua_tointegerx(L, index, &isnum);
#else
	num = lua_tointeger(L, index);
	isnum = (num != 0 || lua_type(L, index) == LUA_TNUMBER);
#endif
	if (isnum) {
		str = NULL; /* Hi, gcc! */

		for (i = 0; options[i].name != NULL; i++) {
			if (num == options[i].value)
				break;
		}
	} else {
		str = luaL_checkstring(L, index);

		for (i = 0; options[i].name != NULL; i++) {
			if (strcmp(str, options[i].name) == 0)
				break;
		}
	}

	if (options[i].name != NULL && (!flags || (options[i].flags & flags)))
		return options[i].value;

	err = isnum ? lua_pushfstring(L, "invalid %s %d", name, num)
		    : lua_pushfstring(L, "invalid %s " LUA_QS, name, str);
	luaL_argerror(L, narg, err);

	/* NOTREACHED */
	return 0;
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

#if 0
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
tosw(lua_State *L, int index, int narg)
{
#if LUA_VERSION_NUM >= 503
	lua_Integer i;
	int isnum;
#else
	lua_Number d;
#endif

	assert(narg > 0);

#if LUA_VERSION_NUM >= 503
	i = lua_tointegerx(L, index, &isnum);

/* XXX
	if (!isnum) {
		switch (lua_type(L, index)) {
		case LUA_TTABLE:
		case LUA_TUSERDATA:
			return usertypetosw(L, index, narg);
		}
	}
*/

	if (!isnum || i != (sljit_sw)i)
		luaL_argerror(L, narg, ERR_NOCONV("sljit_sw"));

	return i;
#else
	d = lua_tonumber(L, index);
	if (d != 0 && d == (sljit_sw)d)
		return d;

	switch (lua_type(L, index)) {
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
		return usertypetosw(L, index, narg);
*/
	default:
		luaL_argerror(L, narg, ERR_NOCONV("sljit_sw"));
	}

	/* NOTREACHED */
	return 0;
#endif
}

/*
 * function sljit.imm(arg)
 *	return { sljit.IMM, arg }
 * end
 * XXX it's more natural to pass numbers.
 */
static int
l_imm(lua_State *L)
{

	lua_createtable(L, 2, 0);
	lua_pushinteger(L, SLJIT_IMM);
	lua_rawseti(L, -2, 1);
	lua_pushvalue(L, 1);
	lua_rawseti(L, -2, 2);

	return 1;
}

/*
 * function sljit.mem0(arg)
 *	return { sljit.MEM, arg }
 * end
 */
static int
l_mem0(lua_State *L)
{

	lua_createtable(L, 2, 0);
	lua_pushinteger(L, SLJIT_MEM0());
	lua_rawseti(L, -2, 1);
	lua_pushvalue(L, 1);
	lua_rawseti(L, -2, 2);

	return 1;
}

/*
 * function sljit.mem1(reg, regw)
 *	return { bit.bor(sljit.MEM, reg), regw }
 * end
 */
static int
l_mem1(lua_State *L)
{
	sljit_si reg;

	reg = checkoption(L, 1, 1, regs, "register", REG_ONLY);

	lua_createtable(L, 2, 0);
	lua_pushinteger(L, SLJIT_MEM1(reg));
	lua_rawseti(L, -2, 1);
	lua_pushvalue(L, 1);
	lua_rawseti(L, -2, 2);

	return 1;
}

/* XXX hmm, how to pass two register? */
static int
l_mem2(lua_State *L)
{
	sljit_si r1, r2;

	r1 = checkoption(L, 1, 1, regs, "register", REG_ONLY);
	r2 = checkoption(L, 2, 2, regs, "register", REG_ONLY);
	lua_pushinteger(L, SLJIT_MEM2(r1, r2));

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
	struct luaSljitCompiler * udata;
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
	struct luaSljitCode * udata;
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

static void
checkreg(lua_State *L, int narg, int flags, sljit_si *reg, sljit_sw *regw)
{

	assert(narg > 0);

	if (lua_type(L, narg) == LUA_TTABLE) {
		lua_rawgeti(L, narg, 1);
		*reg = checkoption(L, -1, narg, regs, "register", flags);
		lua_rawgeti(L, narg, 2);
		*regw = tosw(L, -1, narg);
		lua_pop(L, 2);
	} else {
		*reg = checkoption(L, narg, narg, regs, "register", flags);
		*regw = 0;
	}
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

static int
l_emit_enter(lua_State *L)
{
	struct luaSljitCompiler * comp;
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
	struct luaSljitCompiler * comp;
	sljit_si op;
	int status;

	comp = checkcompiler(L, 1);

	op = checkoption(L, 2, 2, ops0, "opcode", 0);

	status = sljit_emit_op0(comp->compiler, op);
	if (status != SLJIT_SUCCESS)
		return compiler_error(L, "sljit_emit_op0", status);

	lua_pushvalue(L, 1);
	return 1;
}

static int
l_emit_op1(lua_State *L)
{
	struct luaSljitCompiler * comp;
	sljit_sw dstw, srcw;
	sljit_si op, dst, src;
	int status;

	comp = checkcompiler(L, 1);
	op = checkoption(L, 2, 2, ops1, "opcode", 0);
	checkreg(L, 3, REG_ONLY, &dst, &dstw);
	checkreg(L, 4, REG_IMM,  &src, &srcw);

	status = sljit_emit_op1(comp->compiler, op, dst, dstw, src, srcw);
	if (status != SLJIT_SUCCESS)
		return compiler_error(L, "sljit_emit_op1", status);

	lua_pushvalue(L, 1);
	return 1;
}

static int
l_emit_op2(lua_State *L)
{
	struct luaSljitCompiler * comp;
	sljit_sw dstw, src1w, src2w;
	sljit_si op, dst, src1, src2;
	int status;

	comp = checkcompiler(L, 1);
	op = checkoption(L, 2, 2, ops2, "opcode", 0);
	checkreg(L, 3, REG_ONLY, &dst, &dstw);
	checkreg(L, 4, REG_IMM,  &src1, &src1w);
	checkreg(L, 5, REG_IMM,  &src2, &src2w);

	status = sljit_emit_op2(comp->compiler, op,
	    dst, dstw, src1, src1w, src2, src2w);
	if (status != SLJIT_SUCCESS)
		return compiler_error(L, "sljit_emit_op2", status);

	lua_pushvalue(L, 1);
	return 1;
}

static int
l_emit_return(lua_State *L)
{
	struct luaSljitCompiler * comp;
	sljit_sw srcw;
	sljit_si op, src;
	int status;

	comp = checkcompiler(L, 1);
	/* XXX SLJIT_UNUSED opcode */
	op = checkoption(L, 2, 2, ops1, "opcode", OP1_RET);
	checkreg(L, 3, REG_IMM, &src, &srcw);

	status = sljit_emit_return(comp->compiler, op, src, srcw);
	if (status != SLJIT_SUCCESS)
		return compiler_error(L, "sljit_emit_return", status);

	lua_pushvalue(L, 1);
	return 1;
}

static int
l_emit_fast_enter(lua_State *L)
{
	struct luaSljitCompiler * comp;
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
	struct luaSljitCompiler * comp;
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
	struct luaSljitCompiler * comp;
	sljit_sw dstw, offset;
	sljit_si dst;
	int status;

	comp = checkcompiler(L, 1);
	checkreg(L, 2, REG_ONLY, &dst, &dstw);
	offset = tosw(L, 3, 3);

	status = sljit_get_local_base(comp->compiler, dst, dstw, offset);
	if (status != SLJIT_SUCCESS)
		return compiler_error(L, "sljit_get_local_base", status);

	return 0;
}

static int
l_get_compiler_error(lua_State *L)
{
	struct luaSljitCompiler * comp;
	int status;

	comp = checkcompiler(L, 1);

	status = sljit_get_compiler_error(comp->compiler);

	lua_pushinteger(L, status); /* XXX pushstring */

	return 1;
}

static int
l_get_generated_code_size(lua_State *L)
{
	struct luaSljitCompiler * comp;
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
	struct luaSljitJump *udata;
	sljit_si type;

	comp = checkcompiler(L, 1);
	type = checkoption(L, 2, 2, jumptypes, "jump", 0);

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

	udata->jump = sljit_emit_jump(comp->compiler, type);
	if (udata->jump == NULL)
		return luaL_error(L, "sljit.emit_jump() failed");

	return 1;
}

static int
l_emit_cmp(lua_State *L)
{
	struct luaSljitCompiler *comp;
	struct luaSljitJump *udata;
	sljit_sw src1w, src2w;
	sljit_si type, src1, src2;

	comp = checkcompiler(L, 1);
	type = checkoption(L, 2, 2, jumptypes, "comparison", JUMP_CMP);
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
	    type, src1, src1w, src2, src2w);
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
	initval = tosw(L, 3, 3);

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

static int
register_options(lua_State *L, int arg, const struct option *options)
{
	size_t i;

	/* Copy sljit module to the top. */
	if (arg != -1)
		lua_pushvalue(L, arg);

	for (i = 0; options[i].name != NULL; i++) {
		lua_pushstring(L, options[i].name);
		lua_pushinteger(L, options[i].value);
		lua_rawset(L, -3);
	}

	if (arg != -1)
		lua_pop(L, 1);

	return 0;
}

int
luaopen_sljit(lua_State *L)
{

#if LUA_VERSION_NUM <= 501
	luaL_register(L, "sljit", sljit_functions);
#else
	luaL_newlib(L, sljit_functions);
#endif

	register_udata(L, -1, CONST_METATABLE, NULL, const_methods);
	register_udata(L, -1, JUMP_METATABLE,  NULL, jump_methods);
	register_udata(L, -1, LABEL_METATABLE, NULL, label_methods);

	register_udata(L, -1, CODE_METATABLE, code_metafunctions, code_methods);
	register_udata(L, -1, COMP_METATABLE, comp_metafunctions, comp_methods);

	register_options(L, -1, regs);
	register_options(L, -1, ops0);
	register_options(L, -1, ops1);
	register_options(L, -1, ops2);
	register_options(L, -1, jumptypes);
	/* XXX */

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
