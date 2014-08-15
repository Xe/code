cmake_minimum_required(VERSION 2.6)

INCLUDE_DIRECTORIES("../include")

add_executable(raveserv
                channel.c
                raveserv.c
                hash.c
                list.c
                irc.c
                read_line.c)

target_link_libraries(raveserv)

