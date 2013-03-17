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
