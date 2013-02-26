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

#define COMPILER_METATABLE "sljit.compiler"

/* Lua cmod exports only one function. */
int luaopen_sljit(lua_State *L);

/* Compiler userdata. */
struct luaSljitCompiler
{
	struct sljit_compiler *compiler;
};

static int l_create_compiler(lua_State *L)
{
	struct luaSljitCompiler *udata;

	udata = (struct luaSljitCompiler *)
	    lua_newuserdata(L, sizeof(*udata));

	udata->compiler = NULL;

	luaL_getmetatable(L, COMPILER_METATABLE);
	lua_setmetatable(L, -2);

	udata->compiler = sljit_create_compiler();

	if (udata->compiler == NULL)
		return luaL_error(L, "sljit.create_compiler failed");

	return 1;
}

static int l_free_compiler(lua_State *L)
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

static luaL_reg sljit_methods[] = {
	{ "__gc", l_free_compiler },
	{ NULL, NULL }
};

static luaL_reg sljit_functions[] = {
	{ "create_compiler", l_create_compiler },
	{ NULL, NULL }
};

int luaopen_sljit(lua_State *L)
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
