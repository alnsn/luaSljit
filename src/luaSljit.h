#ifndef LUASLJIT_H_INCLUDED
#define LUASLJIT_H_INCLUDED

#include <lua.h>
#include <sljitLir.h>

int luaopen_sljit_api(lua_State *);

struct sljit_compiler *luaSljit_tocompiler(lua_State *, int);
/* XXX luaSljit_pushcompiler */

sljit_sw luaSljit_tosw(lua_State *, int);
void luaSljit_pushsw(lua_State *, sljit_sw);

#endif
