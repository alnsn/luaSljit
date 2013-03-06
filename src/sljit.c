/*-
 * Copyright (c) 2013 Alexander Nasonov.
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

#include <lua.h>
#include <lauxlib.h>

#include <sljitLir.h>

#include <errno.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

/* Metatables. */
#define COMPILER_METATABLE "sljit.compiler"
#define LABEL_METATABLE    "sljit.label"
#define JUMP_METATABLE     "sljit.jump"

/* Errors. */
#define ERR_NOCONV(type) "conversion to " type " failed"

/* Lua cmod exports only one function. */
int luaopen_sljit_api(lua_State *L);

static const sljit_si regs[] = {
	SLJIT_LOCALS_REG,
	SLJIT_PREF_SHIFT_REG, /* XXX get a real name of PREF_SHIFT_REG */
	SLJIT_RETURN_REG,     /* XXX get a real name of RETURN_REG */
	SLJIT_SAVED_EREG1,
	SLJIT_SAVED_EREG2,
	SLJIT_SAVED_REG1,
	SLJIT_SAVED_REG2,
	SLJIT_SAVED_REG3,
	SLJIT_SCRATCH_REG1,
	SLJIT_SCRATCH_REG2,
	SLJIT_SCRATCH_REG3,
	SLJIT_TEMPORARY_EREG1,
	SLJIT_TEMPORARY_EREG2,
	SLJIT_UNUSED,
};

static const char * const regstrings[] = {
	"LOCALS_REG",
	"PREF_SHIFT_REG",
	"RETURN_REG",
	"SAVED_EREG1",
	"SAVED_EREG2",
	"SAVED_REG1",
	"SAVED_REG2",
	"SAVED_REG3",
	"SCRATCH_REG1",
	"SCRATCH_REG2",
	"SCRATCH_REG3",
	"TEMPORARY_EREG1",
	"TEMPORARY_EREG2",
	"UNUSED",
	NULL
};

static const sljit_si ops0[] = {
	SLJIT_BREAKPOINT,
	SLJIT_ISDIV,
	SLJIT_IUDIV,
	SLJIT_NOP,
	SLJIT_SDIV,
	SLJIT_SMUL,
	SLJIT_UDIV,
	SLJIT_UMUL,
};

static const char * const op0strings[] = {
	"BREAKPOINT",
	"ISDIV",
	"IUDIV",
	"NOP",
	"SDIV",
	"SMUL",
	"UDIV",
	"UMUL",
	NULL
};

static const sljit_si ops1[] = {
	SLJIT_CLZ,
	SLJIT_ICLZ,
	SLJIT_IMOV,
	SLJIT_IMOVU,
	SLJIT_IMOVU_SB,
	SLJIT_IMOVU_SH,
	SLJIT_IMOVU_UB,
	SLJIT_IMOVU_UH,
	SLJIT_IMOV_SB,
	SLJIT_IMOV_SH,
	SLJIT_IMOV_UB,
	SLJIT_IMOV_UH,
	SLJIT_INEG,
	SLJIT_INOT,
	SLJIT_MOV,
	SLJIT_MOVU,
	SLJIT_MOVU_P,
	SLJIT_MOVU_SB,
	SLJIT_MOVU_SH,
	SLJIT_MOVU_SI,
	SLJIT_MOVU_UB,
	SLJIT_MOVU_UH,
	SLJIT_MOVU_UI,
	SLJIT_MOV_P,
	SLJIT_MOV_SB,
	SLJIT_MOV_SH,
	SLJIT_MOV_SI,
	SLJIT_MOV_UB,
	SLJIT_MOV_UH,
	SLJIT_MOV_UI,
	SLJIT_NEG,
	SLJIT_NOT,
};

static const char * const op1strings[] = {
	"CLZ",
	"ICLZ",
	"IMOV",
	"IMOVU",
	"IMOVU_SB",
	"IMOVU_SH",
	"IMOVU_UB",
	"IMOVU_UH",
	"IMOV_SB",
	"IMOV_SH",
	"IMOV_UB",
	"IMOV_UH",
	"INEG",
	"INOT",
	"MOV",
	"MOVU",
	"MOVU_P",
	"MOVU_SB",
	"MOVU_SH",
	"MOVU_SI",
	"MOVU_UB",
	"MOVU_UH",
	"MOVU_UI",
	"MOV_P",
	"MOV_SB",
	"MOV_SH",
	"MOV_SI",
	"MOV_UB",
	"MOV_UH",
	"MOV_UI",
	"NEG",
	"NOT",
	NULL
};

/* sljit_emit_return */
static const sljit_si retops[] = {
	SLJIT_IMOV,
	SLJIT_IMOV_SB,
	SLJIT_IMOV_SH,
	SLJIT_IMOV_UB,
	SLJIT_IMOV_UH,
	SLJIT_MOV,
	SLJIT_MOV_P,
	SLJIT_MOV_SB,
	SLJIT_MOV_SH,
	SLJIT_MOV_SI,
	SLJIT_MOV_UB,
	SLJIT_MOV_UH,
	SLJIT_MOV_UI,
	SLJIT_UNUSED, /* XXX hmm, I thought it's a register, not op. */
};

static const char * const retopstrings[] = {
	"IMOV",
	"IMOV_SB",
	"IMOV_SH",
	"IMOV_UB",
	"IMOV_UH",
	"MOV",
	"MOV_P",
	"MOV_SB",
	"MOV_SH",
	"MOV_SI",
	"MOV_UB",
	"MOV_UH",
	"MOV_UI",
	"UNUSED",
	NULL
};

static const sljit_si jumptypes[] = {
	SLJIT_CALL0,
	SLJIT_CALL1,
	SLJIT_CALL2,
	SLJIT_CALL3,
	SLJIT_C_EQUAL,
	SLJIT_C_FLOAT_EQUAL,
	SLJIT_C_FLOAT_GREATER,
	SLJIT_C_FLOAT_GREATER_EQUAL,
	SLJIT_C_FLOAT_LESS,
	SLJIT_C_FLOAT_LESS_EQUAL,
	SLJIT_C_FLOAT_NOT_EQUAL,
	SLJIT_C_FLOAT_ORDERED,
	SLJIT_C_FLOAT_UNORDERED,
	SLJIT_C_GREATER,
	SLJIT_C_GREATER_EQUAL,
	SLJIT_C_LESS,
	SLJIT_C_LESS_EQUAL,
	SLJIT_C_MUL_NOT_OVERFLOW,
	SLJIT_C_MUL_OVERFLOW,
	SLJIT_C_NOT_EQUAL,
	SLJIT_C_NOT_OVERFLOW,
	SLJIT_C_NOT_ZERO,
	SLJIT_C_OVERFLOW,
	SLJIT_C_SIG_GREATER,
	SLJIT_C_SIG_GREATER_EQUAL,
	SLJIT_C_SIG_LESS,
	SLJIT_C_SIG_LESS_EQUAL,
	SLJIT_C_ZERO,
	SLJIT_FAST_CALL,
	SLJIT_JUMP,
};

/* XXX "type can be combined (or'ed) with SLJIT_REWRITABLE_JUMP" */
static const char * const jumptypestrings[] = {
	"CALL0",
	"CALL1",
	"CALL2",
	"CALL3",
	"C_EQUAL",
	"C_FLOAT_EQUAL",
	"C_FLOAT_GREATER",
	"C_FLOAT_GREATER_EQUAL",
	"C_FLOAT_LESS",
	"C_FLOAT_LESS_EQUAL",
	"C_FLOAT_NOT_EQUAL",
	"C_FLOAT_ORDERED",
	"C_FLOAT_UNORDERED",
	"C_GREATER",
	"C_GREATER_EQUAL",
	"C_LESS",
	"C_LESS_EQUAL",
	"C_MUL_NOT_OVERFLOW",
	"C_MUL_OVERFLOW",
	"C_NOT_EQUAL",
	"C_NOT_OVERFLOW",
	"C_NOT_ZERO",
	"C_OVERFLOW",
	"C_SIG_GREATER",
	"C_SIG_GREATER_EQUAL",
	"C_SIG_LESS",
	"C_SIG_LESS_EQUAL",
	"C_ZERO",
	"FAST_CALL",
	"JUMP",
	NULL
};

/*
 * XXX "type can be combined (or'ed) with
 * SLJIT_REWRITABLE_JUMP or SLJIT_INT_OP"
 */
static const sljit_si cmptypes[] = {
	SLJIT_C_EQUAL,
	SLJIT_C_GREATER,
	SLJIT_C_GREATER_EQUAL,
	SLJIT_C_LESS,
	SLJIT_C_LESS_EQUAL,
	SLJIT_C_NOT_EQUAL,
	SLJIT_C_NOT_ZERO,
	SLJIT_C_SIG_GREATER,
	SLJIT_C_SIG_GREATER_EQUAL,
	SLJIT_C_SIG_LESS,
	SLJIT_C_SIG_LESS_EQUAL,
	SLJIT_C_ZERO,
};

static const char * const cmptypestrings[] = {
	"C_EQUAL",
	"C_GREATER",
	"C_GREATER_EQUAL",
	"C_LESS",
	"C_LESS_EQUAL",
	"C_NOT_EQUAL",
	"C_NOT_ZERO",
	"C_SIG_GREATER",
	"C_SIG_GREATER_EQUAL",
	"C_SIG_LESS",
	"C_SIG_LESS_EQUAL",
	"C_ZERO",
	NULL
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

static sljit_sw
tosw(lua_State *L, int narg1, int narg2)
{
	sljit_sw rv;
	const char *s;
	char *se;
	size_t slen;
	lua_Number n;
	unsigned long long ull;
	int badarg, type;
	const int base = 0; /* 8, 10, or 16 */
	bool err;

#define TABLE_WITH_TWO_NUMBERS "table with two uint32_t values"

	rv = 0;
	badarg = narg1;

	type = lua_type(L, narg1);

	switch (type) {
		case LUA_TSTRING:
			s = lua_tolstring(L, narg1, &slen);
			if (s == NULL)
				luaL_argerror(L, badarg, "lua_tolstring failed");

			errno = 0;
			err = false;

			ull = strtoull(s, &se, base);

			if (slen == 0 || se != s + slen)
				err = true;
			if (ull != (sljit_uw)ull)
				err = true;
			if (ull == LLONG_MAX && errno != 0)
				err = true;

			rv = ull;

			if (err)
				luaL_argerror(L, badarg, ERR_NOCONV("sljit_sw"));

			return rv;

		case LUA_TTABLE:
			lua_rawgeti(L, narg1, 1);
			lua_rawgeti(L, narg1, 2);
			narg2 = lua_gettop(L);
			narg1 = narg2 - 1;
			type = lua_type(L, narg1);
			break;

		case LUA_TNUMBER:
			break;

		default:
			luaL_typerror(L, badarg, "number, string or table");
	}

	if (type != LUA_TNUMBER)
		luaL_typerror(L, badarg, TABLE_WITH_TWO_NUMBERS);

	n = lua_tonumber(L, narg1);

	if (n < 0 || n > UINT32_MAX || n != (lua_Integer)n)
		luaL_argerror(L, badarg, ERR_NOCONV("uint32_t"));

	rv = n;

	if (narg2 != narg1) {
		if (lua_type(L, narg2) != LUA_TNUMBER) {
			s = (badarg == narg1) ?
			    "number" : TABLE_WITH_TWO_NUMBERS;
			luaL_typerror(L, (badarg == narg1) ? narg2 : badarg, s);
		}

		n = lua_tonumber(L, narg2);

		if (n < 0 || n > UINT32_MAX || n != (lua_Integer)n) {
			s = TABLE_WITH_TWO_NUMBERS;
			luaL_argerror(L, (badarg == narg1) ? narg2 : badarg, s);
		}

		rv = (rv << 31 << 1) | (sljit_sw)n;
	}

	return rv;
}

/* XXX push userdata. */
static int
l_mem0(lua_State *L)
{

	lua_pushnumber(L, SLJIT_MEM0());

	return 1;
}

/* XXX push userdata. */
static int
l_mem1(lua_State *L)
{
	sljit_si r1;

	r1 = regs[luaL_checkoption(L, 1, NULL, regstrings)];
	lua_pushnumber(L, SLJIT_MEM1(r1));

	return 1;
}

/* XXX push userdata. */
static int
l_mem2(lua_State *L)
{
	sljit_si r1, r2;

	r1 = regs[luaL_checkoption(L, 1, NULL, regstrings)];
	r2 = regs[luaL_checkoption(L, 2, NULL, regstrings)];
	lua_pushnumber(L, SLJIT_MEM2(r1, r2));

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
l_create_compiler(lua_State *L)
{
	struct luaSljitCompiler *udata;

	udata = (struct luaSljitCompiler *)
	    lua_newuserdata(L, sizeof(struct luaSljitCompiler));

	udata->compiler = NULL;

	luaL_getmetatable(L, COMPILER_METATABLE);
	lua_setmetatable(L, -2);

	udata->compiler = sljit_create_compiler();

	if (udata->compiler == NULL)
		return luaL_error(L, "sljit.create_compiler failed");

	return 1;
}

static int
gc_compiler(lua_State *L)
{
	struct luaSljitCompiler * udata;

	udata = (struct luaSljitCompiler *)
	    luaL_checkudata(L, 1, COMPILER_METATABLE);

	if (udata->compiler != NULL) {
		sljit_free_compiler(udata->compiler);
		udata->compiler = NULL;
	}

	return 0;
}

static int
compiler_error(lua_State *L, const char *fname, int status)
{

	/* XXX convert status to string. */
	return luaL_error(L, "%s failed with %d", fname, status);
}

static sljit_si
checkreg(lua_State *L, int narg)
{

	/* XXX return userdata from mem0, mem1 and mem2 */
	if (lua_isnumber(L, narg))
		return luaL_checkint(L, narg);

	return regs[luaL_checkoption(L, narg, NULL, regstrings)];
}

static struct luaSljitCompiler *
checkcompiler(lua_State *L, int narg)
{
	struct luaSljitCompiler *comp;

	comp = (struct luaSljitCompiler *)
	    luaL_checkudata(L, narg, COMPILER_METATABLE);

	if (comp->compiler == NULL)
		luaL_error(L, "sljit.compiler object is dead");

	return comp;
}

static struct luaSljitJump *
checkjump(lua_State *L, int narg)
{
	struct luaSljitJump *jump;

	jump = (struct luaSljitJump *)
	    luaL_checkudata(L, narg, JUMP_METATABLE);

	if (jump->jump == NULL)
		luaL_error(L, "sljit.jump object is dead");

	return jump;
}

static struct luaSljitLabel *
checklabel(lua_State *L, int narg)
{
	struct luaSljitLabel *label;

	label = (struct luaSljitLabel *)
	    luaL_checkudata(L, narg, LABEL_METATABLE);

	if (label->label == NULL)
		luaL_error(L, "sljit.label object is dead");

	return label;
}

static int
l_emit_enter(lua_State *L)
{
	struct luaSljitCompiler * comp;
	int args, scratches, generals, local_size;
	int last, status;

	comp = checkcompiler(L, 1);

	if (lua_type(L, 2) != LUA_TTABLE) {
		last = 1;
	} else {
		last = lua_gettop(L);
		lua_pushstring(L, "args");
		lua_gettable(L, 2);
		lua_pushstring(L, "scratches");
		lua_gettable(L, 2);
		lua_pushstring(L, "generals");
		lua_gettable(L, 2);
		lua_pushstring(L, "local_size");
		lua_gettable(L, 2);
	}

	/*
	 * XXX Report type errors when the first argument is a
	 * table. Implement default values.
	 */
	args       = luaL_checkint(L, last + 1);
	scratches  = luaL_checkint(L, last + 2);
	generals   = luaL_checkint(L, last + 3);
	local_size = luaL_checkint(L, last + 4);

	status = sljit_emit_enter(comp->compiler,
	    args, scratches, generals, local_size);

	if (status != SLJIT_SUCCESS)
		return compiler_error(L, "sljit_emit_enter", status);

	return 0;
}

static int
l_emit_op0(lua_State *L)
{
	struct luaSljitCompiler * comp;
	sljit_si op;
	int status;

	comp = checkcompiler(L, 1);

	op = ops0[luaL_checkoption(L, 2, NULL, op0strings)];

	status = sljit_emit_op0(comp->compiler, op);

	if (status != SLJIT_SUCCESS)
		return compiler_error(L, "sljit_emit_op0", status);

	return 0;
}

static int
l_emit_op1(lua_State *L)
{
	struct luaSljitCompiler * comp;
	sljit_sw dstw, srcw;
	sljit_si op, dst, src;
	int status;

	comp = checkcompiler(L, 1);

	op   = ops1[luaL_checkoption(L, 2, NULL, op1strings)];
	dst  = checkreg(L, 3);
	dstw = tosw(L, 4, 4);
	src  = checkreg(L, 5);
	srcw = tosw(L, 6, 6);

	status = sljit_emit_op1(comp->compiler,
	    op, dst, dstw, src, srcw);

	if (status != SLJIT_SUCCESS)
		return compiler_error(L, "sljit_emit_op1", status);

	return 0;
}

static int
l_emit_return(lua_State *L)
{
	struct luaSljitCompiler * comp;
	sljit_sw srcw;
	sljit_si op, src;
	int status;

	comp = checkcompiler(L, 1);

	op   = retops[luaL_checkoption(L, 2, NULL, retopstrings)];
	src  = checkreg(L, 3);
	srcw = tosw(L, 4, 4);

	status = sljit_emit_return(comp->compiler, op, src, srcw);

	if (status != SLJIT_SUCCESS)
		return compiler_error(L, "sljit_emit_return", status);

	return 0;
}

static int
l_emit_fast_enter(lua_State *L)
{
	struct luaSljitCompiler * comp;
	sljit_sw dstw;
	sljit_si dst;
	int status;

	comp = checkcompiler(L, 1);

	dst  = checkreg(L, 2);
	dstw = tosw(L, 3, 3);

	status = sljit_emit_fast_enter(comp->compiler, dst, dstw);

	if (status != SLJIT_SUCCESS)
		return compiler_error(L, "sljit_emit_fast_enter", status);

	return 0;
}

static int
l_emit_fast_return(lua_State *L)
{
	struct luaSljitCompiler * comp;
	sljit_sw srcw;
	sljit_si src;
	int status;

	comp = checkcompiler(L, 1);

	src  = checkreg(L, 2);
	srcw = tosw(L, 3, 3);

	status = sljit_emit_fast_return(comp->compiler, src, srcw);

	if (status != SLJIT_SUCCESS)
		return compiler_error(L, "sljit_emit_fast_return", status);

	return 0;
}

static int
l_get_local_base(lua_State *L)
{
	struct luaSljitCompiler * comp;
	sljit_sw dstw, offset;
	sljit_si dst;
	int status;

	comp = checkcompiler(L, 1);

	dst    = checkreg(L, 2);
	dstw   = tosw(L, 3, 3);
	offset = tosw(L, 4, 4);

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

	lua_pushnumber(L, status); /* XXX pushstring */

	return 1;
}

static int
l_get_generated_code_size(lua_State *L)
{
	struct luaSljitCompiler * comp;
	sljit_uw sz;

	comp = checkcompiler(L, 1);

	sz = sljit_get_generated_code_size(comp->compiler);

	lua_pushnumber(L, sz);

	return 1;
}

static int
l_emit_jump(lua_State *L)
{
	struct luaSljitCompiler *comp;
	struct luaSljitJump *udata;
	sljit_si type;

	comp = checkcompiler(L, 1);
	type = jumptypes[luaL_checkoption(L, 2, NULL, jumptypestrings)];

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
	lua_rawseti(L, -2, 1);
	lua_setfenv(L, -2);

	udata->jump = sljit_emit_jump(comp->compiler, type);
	if (udata->jump == NULL)
		return luaL_error(L, "emit_jump failed");

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
	type = cmptypes[luaL_checkoption(L, 2, NULL, cmptypestrings)];
	src1 = checkreg(L, 3);
	src1w = tosw(L, 4, 4);
	src2 = checkreg(L, 5);
	src2w = tosw(L, 6, 6);

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
	lua_rawseti(L, -2, 1);
	lua_setfenv(L, -2);

	udata->jump = sljit_emit_cmp(comp->compiler,
	    type, src1, src1w, src2, src2w);
	if (udata->jump == NULL)
		return luaL_error(L, "emit_cmp failed");

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
	lua_rawseti(L, -2, 1);
	lua_setfenv(L, -2);

	udata->label = sljit_emit_label(comp->compiler);
	if (udata->label == NULL)
		return luaL_error(L, "emit_label failed");

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

	return 0;
}


static int
gc_jump(lua_State *L)
{
	struct luaSljitJump * udata;

	udata = (struct luaSljitJump *)
	    luaL_checkudata(L, 1, JUMP_METATABLE);

	udata->jump = NULL;

	return 0;
}

static int
gc_label(lua_State *L)
{
	struct luaSljitLabel * udata;

	udata = (struct luaSljitLabel *)
	    luaL_checkudata(L, 1, LABEL_METATABLE);

	udata->label = NULL;

	return 0;
}

static luaL_reg compiler_methods[] = {
	{ "emit_cmp",                l_emit_cmp                },
	{ "emit_enter",              l_emit_enter              },
	{ "emit_fast_enter",         l_emit_fast_enter         },
	{ "emit_fast_return",        l_emit_fast_return        },
	{ "emit_jump",               l_emit_jump               },
	{ "emit_label",              l_emit_label              },
	{ "emit_op0",                l_emit_op0                },
	{ "emit_op1",                l_emit_op1                },
	{ "emit_return",             l_emit_return             },
	{ "get_compiler_error",      l_get_compiler_error      },
	{ "get_generated_code_size", l_get_generated_code_size },
	{ "get_local_base",          l_get_local_base          },
	{ NULL, NULL }
};

static luaL_reg jump_methods[] = {
	{ "set_label", l_set_label },
	{ NULL, NULL }
};

static luaL_reg label_methods[] = {
	{ NULL, NULL }
};

static luaL_reg sljit_functions[] = {
	{ "create_compiler", l_create_compiler },
	{ "mem0",            l_mem0            },
	{ "mem1",            l_mem1            },
	{ "mem2",            l_mem2            },
	{ "unaligned",       l_unaligned       },
	{ NULL, NULL }
};

static int
register_methods(lua_State *L, const char *tname,
    int (*gc)(lua_State *), const luaL_reg *methods)
{

	luaL_newmetatable(L, tname);

	lua_pushstring(L, "__gc");
	lua_pushcfunction(L, gc);
	lua_rawset(L, -3);

	lua_pushstring(L, "__index");

	/* XXX luaL_register is deprecated in version 5.2. */
	lua_newtable(L);
	luaL_register(L, NULL, methods);

	lua_rawset(L, -3);

	return 0;
}

int
luaopen_sljit_api(lua_State *L)
{

	register_methods(L, JUMP_METATABLE, &gc_jump, jump_methods);
	register_methods(L, LABEL_METATABLE, &gc_label, label_methods);
	register_methods(L, COMPILER_METATABLE, &gc_compiler, compiler_methods);

	/* XXX luaL_register is deprecated in version 5.2. */
	luaL_register(L, "sljit", sljit_functions);

	return 1;
}
