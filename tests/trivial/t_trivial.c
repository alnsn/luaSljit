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
#include <lauxlib.h>

#include <err.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>


const char prog[] =
	"local res = ...                                             \n"
	"local c = sljit.create_compiler()                           \n"
	"c:emit_enter{args=0, scratches=1, generals=0, local_size=0} \n"
	"c:emit_op1('MOV', 'SCRATCH_REG1', '0', 'IMM', res)          \n"
	"c:emit_return('MOV', 'SCRATCH_REG1', 0)                     \n"
	"return c";

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
	}
	return "Unknown error";
}

int main(int argc, char *argv[])
{
	lua_State *L;
	struct sljit_compiler *compiler;
	myfunc fn;
	sljit_sw res;
	const char *errstr, *errmsg;
	int status, nargs, nresults, errfunc;

	L = luaL_newstate();
	luaopen_sljit_api(L);

	status = luaL_loadbuffer(L, prog, sizeof(prog) - 1, "prog");

	if ((errstr = luaerrstr(status)) != NULL) {
		lua_close(L);
		errx(EXIT_FAILURE, "Error loading Lua chunk: %s", errstr);
	}

	nargs = 1;
	luaSljit_pushsw(L, -1);

	nresults = 1;
	errfunc = 0;

	status = lua_pcall(L, nargs, nresults, errfunc);

	if ((errstr = luaerrstr(status)) != NULL) {
		errmsg = lua_tostring(L, -1);
		errmsg = strdup(errmsg ? errmsg : "");
		if (errmsg == NULL)
			err(EXIT_FAILURE, "strdup");
		lua_close(L);
		errx(EXIT_FAILURE, "Error running Lua chunk: %s\n%s",
		    errstr, errmsg);
	}

	compiler = luaSljit_tocompiler(L, -1);

	if (compiler == NULL) {
		lua_close(L);
		errx(EXIT_FAILURE, "Compiler is dead");
	}

	fn = (myfunc)sljit_generate_code(compiler);

	printf("Generated code size: %ju\n",
	    (uintmax_t)sljit_get_generated_code_size(compiler));

	/* The compiler can be garbage-collected now. */
	lua_pop(L, 1);

	res = fn();

	printf("Generated function returned: %jd\n", (intmax_t)res);

	if (res != -1) {
		sljit_free_code(fn);
		lua_close(L);
		errx(EXIT_FAILURE, "Generated function returned wrong value");
	}

	sljit_free_code(fn);
	lua_close(L);

	return EXIT_SUCCESS;
}
