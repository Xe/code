cmake_minimum_required (VERSION 2.6)
project (sancara)

configure_file (
    "include/config.h.in"
    "include/config.h"
)

set (CMAKE_C_FLAGS "-g")

FIND_PACKAGE(Curses REQUIRED)
FIND_PACKAGE(Lua51 REQUIRED)
INCLUDE_DIRECTORIES(${LUA_INCLUDE_DIR} ${CURSES_INCLUDE_DIR} "include")

add_subdirectory("src")
