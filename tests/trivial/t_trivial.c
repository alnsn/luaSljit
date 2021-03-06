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

#include <luaSljit.h>

#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>

#include <err.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>


const char chunk[] =
	"local sljit = require 'sljit'              \n"
	"local res = ...                            \n"
	"print('Lua: return ' .. res)               \n"
	"return sljit.create_compiler()             \n"
	"    :emit_enter{scratches=1}               \n"
	"    :emit_op1('MOV', 'R0', sljit.imm(res)) \n"
	"    :emit_return('MOV', 'R0')              \n";

typedef sljit_sw (*myfunc)(void);

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

static int
run_test(lua_State *L)
{
	struct sljit_compiler *compiler;
	myfunc fn;
	sljit_sw res;
	const sljit_sw generate_ret = INT64_MIN + 123456;
	const char *errstr;
	int status;

	printf("C  : return %jd\n", (intmax_t)generate_ret);

#if LUA_VERSION_NUM >= 502
	luaL_openlibs(L); // "print" for debugging
	//luaL_requiref(L, "package", &luaopen_package, false);
	luaL_requiref(L, "sljit", &luaopen_sljit, false);
#else
	luaL_openlibs(L);
	luaopen_sljit(L);
#endif

	status = luaL_loadbuffer(L, chunk, sizeof(chunk) - 1, "chunk");

	if ((errstr = luaerrstr(status)) != NULL)
		return luaL_error(L, "Error loading Lua chunk: %s", errstr);

	lua_pushinteger(L, generate_ret);
	lua_call(L, 1, 1);

	compiler = luaSljit_tocompiler(L, -1);

	if (compiler == NULL)
		return luaL_error(L, "Compiler is dead (impossible!)");

	fn = (myfunc)sljit_generate_code(compiler);

	/* The compiler can be garbage-collected now. */
	lua_pop(L, 1);

	res = fn();

	printf("Generated function returned: %jd\n", (intmax_t)res);

	if (res != generate_ret) {
		sljit_free_code(fn);
		return luaL_error(L, "Generated function returned wrong value");
	}

	sljit_free_code(fn);

	return 0;
}

int main(int argc, char *argv[])
{
	lua_State *L;
	const char *errstr, *errmsg;
	int status;

	L = luaL_newstate();
	if (L == NULL)
		errx(EXIT_FAILURE, "Failed to create lua_State");

	lua_pushcfunction(L, &run_test);
	status = lua_pcall(L, 0, 0, 0);

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
