cmake_minimum_required (VERSION 2.6)
project (raveserv)

configure_file (
    "include/config.h.in"
    "include/config.h"
)

set (CMAKE_C_FLAGS "-g -std=c99")

INCLUDE_DIRECTORIES("include")

add_subdirectory("src")
