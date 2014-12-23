/*-
 * Copyright (c) 2014 Alexander Nasonov.
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

#include <err.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

/* XXX Only works on NetBSD */
#include <sys/cdefs.h>
#include <sys/bitops.h>

typedef uint32_t (*myfunc)(uint32_t);

const char chunk[] =
	"local sljit = require 'sljit'                      \n"
	"local mul, sh1, sh2 = ...                          \n"
	"print('Lua:', ...)                                 \n"
	"print(sljit.word_width())\n"
	"return sljit.create_compiler()                     \n"
	"    :emit_enter{args=1, saveds=1, scratches=1}     \n"
	"    :emit_op2('MUL', 'R0', 'S0', sljit.imm(mul))   \n"
	"    :emit_op2('LSHR', 'R0', 'R0', sljit.imm(32))   \n"
	"    :emit_op2('ISUB', 'S0', 'S0', 'R0')            \n"
	"    :emit_op2('ILSHR', 'S0', 'S0', sljit.imm(sh1)) \n"
	"    :emit_op2('IADD', 'R0', 'R0', 'S0')            \n"
	"    :emit_op2('ILSHR', 'R0', 'R0', sljit.imm(sh2)) \n"
	"    :emit_return('MOV_UI', 'R0', 0)                \n"
	"    :generate_code()";

#if 0 /* XXX 32bit */
	"    :emit_enter{args=1, saveds=1, scratches=2}     \n"
	"    :emit_op1('MOV', 'R0', 'S0')                   \n"
	"    :emit_op1('MOV', 'R1', sljit.imm(mul))         \n"
	"    :emit_op0('UMUL') -- R0*R1 => R1:R0            \n"
	"    :emit_op2('ISUB', 'R0', 'S0', 'R1')            \n"
	"    :emit_op2('ILSHR', 'R0', 'R0', sljit.imm(sh1)) \n"
	"    :emit_op2('IADD', 'R0', 'R1', 'R0')            \n"
	"    :emit_op2('ILSHR', 'R0', 'R0', sljit.imm(sh2)) \n"
	"    :emit_return('MOV_UI', 'R0', 0)";
#endif

static const char *
luaerrstr(int status)
{

	switch (status) {
		case 0:             return NULL;
		case LUA_ERRERR:    return "LUA_ERRERR";
		case LUA_ERRMEM:    return "LUA_ERRMEM";
		case LUA_ERRRUN:    return "LUA_ERRRUN";
		case LUA_ERRSYNTAX: return "LUA_ERRSYNTAX";
#if LUA_VERSION_NUM >= 502
		case LUA_ERRGCMM:   return "LUA_ERRGCMM";
#endif
	}
	return "Unknown error";
}

#define DIVISOR UINT32_C(17)
uint32_t mul;
uint8_t sh1, sh2;

__noinline uint32_t div17(uint32_t);
uint32_t
div17(uint32_t v)
{

	return v / DIVISOR;
}

__noinline uint32_t divide(uint32_t);
uint32_t
divide(uint32_t v)
{

	//return v / DIVISOR;
	return fast_divide32(v, DIVISOR, mul, sh1, sh2);
}

int main(int argc, char *argv[])
{
	lua_State *L;
	const char *errstr, *errmsg;
	int status;
	//struct sljit_compiler *compiler;
	myfunc fn;
	uint32_t arg, res;

	fast_divide32_prepare(DIVISOR, &mul, &sh1, &sh2);

	printf("C  :\t%ju\t%u\t%u\n", (uintmax_t)mul, sh1, sh2);

	L = luaL_newstate();
	if (L == NULL)
		errx(EXIT_FAILURE, "Failed to create lua_State");

#if LUA_VERSION_NUM >= 502
	luaL_openlibs(L);
	//luaL_requiref(L, "package", &luaopen_package, false);
	luaL_requiref(L, "sljit", &luaopen_sljit, false);
#else
	luaL_openlibs(L);
	luaopen_sljit(L);
#endif

	/* XXX don't run it unprotected from main() */
	status = luaL_loadbuffer(L, chunk, sizeof(chunk) - 1, "chunk");

	if ((errstr = luaerrstr(status)) != NULL)
		return luaL_error(L, "Error loading Lua chunk: %s", errstr);

	lua_pushinteger(L, mul);
	lua_pushinteger(L, sh1);
	lua_pushinteger(L, sh2);
	lua_call(L, 3, 1);

	//compiler = luaSljit_tocompiler(L, -1);
	//compiler = luaSljit_get_compiler(L, -1);
	//if (compiler == NULL)
	//	return luaL_error(L, "Compiler is dead (impossible!)");

	fn = (myfunc)luaSljit_tocode(L, -1);
	//fn = (myfunc)sljit_generate_code(compiler);

	arg = DIVISOR * 13 + 7;
	res = fn(arg);

	printf("Generated function returned: %jd\n", (intmax_t)res);

	if (res != divide(arg) || res != arg / DIVISOR) {
		sljit_free_code(fn);
		return luaL_error(L, "Generated function returned wrong value");
	}

	//sljit_free_code(fn);

	if ((errstr = luaerrstr(status)) != NULL) {
		errmsg = lua_tostring(L, -1);
		errmsg = strdup(errmsg ? errmsg : "");
		lua_close(L);
		if (errmsg == NULL)
			errx(EXIT_FAILURE, "strdup");
		errx(EXIT_FAILURE, "Error while running the test: %s\n%s",
		    errstr, errmsg);
	}

	lua_close(L);

	return EXIT_SUCCESS;
}
