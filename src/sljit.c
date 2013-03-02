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
#define JUMP_METATABLE "sljit.jump"

/* Errors. */
#define ERR_COMPILER_DEAD "compiler is dead"
#define ERR_NOCONV(type) "conversion to " type " failed"

/* Lua cmod exports only one function. */
int luaopen_sljit(lua_State *L);

static const sljit_si regs[] = {
	SLJIT_LOCALS_REG,
	SLJIT_RETURN_REG,
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
l_free_compiler(lua_State *L)
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

static struct luaSljitCompiler *
checkcompiler(lua_State *L, int narg)
{
	struct luaSljitCompiler * udata;

	udata = (struct luaSljitCompiler *)
	    luaL_checkudata(L, narg, COMPILER_METATABLE);

	if (udata->compiler == NULL)
		luaL_error(L, ERR_COMPILER_DEAD);

	return udata;
}

static int
l_emit_enter(lua_State *L)
{
	struct luaSljitCompiler * udata;
	int args, temporaries, generals, local_size, status;

	udata = checkcompiler(L, 1);

	args        = luaL_checkint(L, 2);
	temporaries = luaL_checkint(L, 3);
	generals    = luaL_checkint(L, 4);
	local_size  = luaL_checkint(L, 5);

	status = sljit_emit_enter(udata->compiler,
	    args, temporaries, generals, local_size);

	if (status != SLJIT_SUCCESS)
		return compiler_error(L, "sljit_emit_enter", status);

	return 0;
}

static int
l_emit_op0(lua_State *L)
{
	struct luaSljitCompiler * udata;
	sljit_si op;
	int status;

	udata = checkcompiler(L, 1);

	op = ops0[luaL_checkoption(L, 2, NULL, op0strings)];

	status = sljit_emit_op0(udata->compiler, op);

	if (status != SLJIT_SUCCESS)
		return compiler_error(L, "sljit_emit_op1", status);

	return 0;
}

static int
l_emit_op1(lua_State *L)
{
	struct luaSljitCompiler * udata;
	sljit_sw dstw, srcw;
	sljit_si op, dst, src;
	int status;

	udata = checkcompiler(L, 1);

	op   = ops1[luaL_checkoption(L, 2, NULL, op1strings)];
	dst  = regs[luaL_checkoption(L, 3, NULL, regstrings)];
	dstw = tosw(L, 4, 4);
	src  = regs[luaL_checkoption(L, 5, NULL, regstrings)];
	srcw = tosw(L, 6, 6);

	status = sljit_emit_op1(udata->compiler,
	    op, dst, dstw, src, srcw);

	if (status != SLJIT_SUCCESS)
		return compiler_error(L, "sljit_emit_op1", status);

	return 0;
}

static luaL_reg sljit_methods[] = {
	{ "emit_enter", l_emit_enter },
	{ "emit_op0", l_emit_op0 },
	{ "emit_op1", l_emit_op1 },
	{ "__gc", l_free_compiler },
	{ NULL, NULL }
};

static luaL_reg sljit_functions[] = {
	{ "create_compiler", l_create_compiler },
	{ NULL, NULL }
};

int
luaopen_sljit(lua_State *L)
{

	luaL_newmetatable(L, COMPILER_METATABLE);

	/* metatable.__index = metatable */
	lua_pushvalue(L, -1);
	lua_setfield(L, -2, "__index");

	/* XXX luaL_register is deprecated in version 5.2. */
	luaL_register(L, NULL, sljit_methods);
	luaL_register(L, "sljit", sljit_functions);

	return 1;
}
