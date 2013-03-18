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

#include "bpfjit.h"
#include "luaSljit.h"

#include <lua.h>
#include <lauxlib.h>

/* To be moved to .lua file. */
const char chunk[] =
	"local prog = ...                                            \n"
	"local c = sljit.create_compiler()                           \n"
	"c:emit_enter{args=3, scratches=1, generals=3, local_size=0} \n"
	"c:emit_op1('MOV', 'SCRATCH_REG1', '0', 'IMM', 0)            \n"
	"c:emit_return('MOV', 'SCRATCH_REG1', 0)                     \n"
	"return c";


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

/*
 * Pushes a table { code=insn.code, jt=insn.jt, jf=insn.jf, k=insn.k }.
 * XXX k is uint32_t (and int32_t on some systems).
 */
static void
push_insn(lua_State *L, struct bpf_insn *insn)
{

	lua_createtable(L, 0, 4);
	lua_pushinteger(L, insn->code);
	lua_setfield(L, -2, "code");
	lua_pushinteger(L, insn->jt);
	lua_setfield(L, -2, "jt");
	lua_pushinteger(L, insn->jf);
	lua_setfield(L, -2, "jf");
	lua_pushinteger(L, insn->k);
	lua_setfield(L, -2, "k");
}

/*
 * Pushes an array of instructions.
 */
static void
push_insns(lua_State *L, struct bpf_program *prog)
{
	size_t i;
	size_t insn_count;

	insn_count = prog->bf_len;

	lua_createtable(L, insn_count, 0);

	for (i = 0; i < insn_count; i++) {
		push_insn(L, &prog->bf_insns[i]);
		lua_rawseti(L, -2, i + 1);
	}
}

/*
 * Reads bpf_program which is passed as light userdata in the first
 * argument, generates code and pushes sljit.compiler userdata.
 */
static int
generate_code(lua_State *L)
{
	struct bpf_program *prog;
	const char *errstr;
	int status;

	prog = (struct bpf_program *)lua_touserdata(L, 1);

	luaSljit_open(L);

	status = luaL_loadbuffer(L, chunk, sizeof(chunk) - 1, "chunk");

	if ((errstr = luaerrstr(status)) != NULL)
		return luaL_error(L, "Error loading bpfjit chunk: %s", errstr);

	push_insns(L, prog);
	lua_call(L, 1, 1);

	return 1;
}

bpfjit_function_t
bpfjit_generate_code(struct bpf_insn *insns, size_t insn_count)
{
	lua_State *L;
	struct bpf_program prog;
	int status;

	prog.bf_len = insn_count;
	prog.bf_insns = insns;

	L = luaL_newstate();

	status = lua_cpcall(L, &generate_code, &prog);

	lua_close(L);

	if (status != 0)
		return NULL;

	return NULL;
}
