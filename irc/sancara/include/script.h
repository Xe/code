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

#ifndef SCRIPT_H

#define SCRIPT_H

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include "sancara.h"

void init_lua();
void init_lua_functions(lua_State *);
void destroy_lua();
char * get_global_string(lua_State *, char *);
int get_global_int(lua_State *, char *);
void read_config(Config *);
int do_lua_file(char *, char *);

int l_topicbar(lua_State *);
int l_statusbar(lua_State *);
int l_prompt(lua_State *);

#define Lua_Function(state, func, name) lua_pushcfunction(state, func); lua_setglobal(state, name)

#endif
