#include <string.h>
#include <lauxlib.h>
#include <lua.h>
#include <lualib.h>

#include "irc.h"

int l_irc_parse(lua_State *l) {
	int i;
	IRCLine * line;

	char * arg = luaL_checkstring(l, 1);
	line = parse_line(arg);

	lua_createtable(l, 0, 3);

	lua_pushstring(l, line->source);
	lua_setfield(l, -2, "source");

	lua_pushstring(l, line->verb);
	lua_setfield(l, -2, "verb");

	lua_createtable(l, line->argc, 0);
	for (i = 0; i < line->argc; i++) {
		lua_pushnumber(l, i+1);
		lua_pushstring(l, line->argv[i]);
		lua_settable(l, -3);
	}
	lua_setfield(l, -2, "argv");

	line_destroy(line);

	return 1;
}

static const struct luaL_reg ircmess[] = {
	{"parse", l_irc_parse},
	{NULL, NULL}
};

int luaopen_ircmess(lua_State * l) {
	luaL_register(l, "ircmess", ircmess);
	return 1;
}
