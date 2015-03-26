cmake_minimum_required(VERSION 2.6)

INCLUDE_DIRECTORIES(${LUA_INCLUDE_DIR} ${CURSES_INCLUDE_DIR} "../include")

add_executable(sancara
                sancara.c
                script.c
                irc.c
                ui.c
                read_line.c
                list.c list_node.c list_iterator.c
                hash.c)

target_link_libraries(sancara ${LUA_LIBRARIES} ${CURSES_LIBRARIES})

