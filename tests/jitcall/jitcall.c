/*-
 * Copyright (c) 2015 Alexander Nasonov.
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
#include <luaSljit.h>

int luaopen_jitcall(lua_State *);

static int
l_call(lua_State *L)
{
	sljit_sw args[3];
	void *code;
	sljit_sw (*call0)(void);
	sljit_sw (*call1)(sljit_sw);
	sljit_sw (*call2)(sljit_sw, sljit_sw);
	sljit_sw (*call3)(sljit_sw, sljit_sw, sljit_sw);
	sljit_sw res = 0;
	int i, nargs;

	code = luaSljit_tocode(L, 1);

	nargs = lua_gettop(L) - 1;
	if (nargs > 3)
		nargs = 3;

	for (i = 1; i <= nargs; i++) {
		switch (lua_type(L, i+1)) {
			case LUA_TSTRING:
				args[i-1] = (sljit_sw)lua_tostring(L, i+1);
				break;
			case LUA_TBOOLEAN:
				args[i-1] = (sljit_sw)lua_toboolean(L, i+1);
				break;
			case LUA_TFUNCTION:
			case LUA_TLIGHTUSERDATA:
			case LUA_TTHREAD:
			case LUA_TUSERDATA:
				args[i-1] = (sljit_sw)lua_topointer(L, i+1);
				break;
			case LUA_TNUMBER:
			default:
				args[i-1] = (sljit_sw)lua_tonumber(L, i+1);
				break;
		}
	}

	switch (nargs) {
		case 0:
			call0 = code;
			res = call0();
			break;
		case 1:
			call1 = code;
			res = call1(args[0]);
			break;
		case 2:
			call2 = code;
			res = call2(args[0], args[1]);
			break;
		case 3:
			call3 = code;
			res = call3(args[0], args[1], args[2]);
			break;
	}

	lua_pushinteger(L, res);
	return 1;
}

static luaL_Reg jicall_functions[] = {
	{ "call",  l_call },
	// XXX fcall?
	{ NULL, NULL }
};

int
luaopen_jitcall(lua_State *L)
{
#if LUA_VERSION_NUM <= 501
	luaL_register(L, "jitcall", jicall_functions);
#else
	luaL_newlib(L, jicall_functions);
#endif

	return 1;
}
