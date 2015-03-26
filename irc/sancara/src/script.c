/*
Copyright (c) 2013-2014, Christine Dodrill
All rights reserved.

This software is provided 'as-is', without any express or implied
warranty. In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not
   claim that you wrote the original software. If you use this software
   in a product, an acknowledgment in the product documentation would be
   appreciated but is not required.

2. Altered source versions must be plainly marked as such, and must not be
   misrepresented as being the original software.

3. This notice may not be removed or altered from any source
   distribution.

 */

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include <ncurses.h>

#include "irc.h"
#include "sancara.h"
#include "script.h"
#include "ui.h"

// Lua interpreter
lua_State * l;

void init_lua() {
	l = lua_open();

	init_lua_functions(l);

	luaL_dofile(l, "config.lua");
}

void init_lua_functions(lua_State * l) {
	luaL_openlibs(l);

	Lua_Function(l, l_topicbar, "topic");
	Lua_Function(l, l_statusbar, "status");
	Lua_Function(l, l_prompt, "prompt");
}

void destroy_lua() {
	lua_close(l);
}

char * get_global_string(lua_State * l, char * name) {
	lua_getglobal(l, name);
	return (char *) lua_tostring(l, -1);
}

int get_global_int(lua_State * l, char * name) {
	lua_getglobal(l, name);
	return lua_tointeger(l, -1);
}

void read_config(Config * conf) {
	lua_getglobal(l, "settings");
	lua_getfield(l, 1, "nick");
	lua_getfield(l, 1, "user");
	lua_getfield(l, 1, "real");
	lua_getfield(l, 1, "host");
	lua_getfield(l, 1, "port");

	conf->nick = (char *) luaL_checkstring(l, -5);
	conf->user = (char *) luaL_checkstring(l, -4);
	conf->real = (char *) luaL_checkstring(l, -3);
	conf->host = (char *) luaL_checkstring(l, -2);
	conf->port = luaL_checkint(l, -1);
}

int do_lua_file(char * name, char * func) {
	int dofile;
	lua_State * lstate;

	lstate = lua_open();
	init_lua_functions(lstate);

	dofile = luaL_dofile(lstate, name);

	if(dofile == 0) {
		lua_getglobal(lstate, func);
		lua_call(l, 0, 0);
	} else {
		lua_close(lstate);
		return -1;
	}

	lua_close(lstate);

	return 0;
}

int l_topicbar(lua_State *l) {
	const char * topic;
	if (lua_isstring(l, -1)) {
		topic = lua_tostring(l, -1);
		draw_topicbar((char *)topic);
		lua_pushboolean(l, TRUE);
	}
	return 0;
}

int l_statusbar(lua_State *l) {
	const char * status;
	if (lua_isstring(l, -1)) {
		status = lua_tostring(l, -1);
		draw_statusbar((char *)status);
		lua_pushboolean(l, TRUE);
	}
	return 0;
}

int l_prompt(lua_State *l) {
	const char * prompt;
	if (lua_isstring(l, -1)) {
		prompt = lua_tostring(l, -1);
		draw_topicbar((char *)prompt);
		lua_pushboolean(l, TRUE);
	}
	return 0;
}

