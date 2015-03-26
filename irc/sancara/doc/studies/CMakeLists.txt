cmake_minimum_required(VERSION 2.6)

add_executable(irc irc.c)

target_link_libraries(irc ${CURSES_LIBRARIES})

add_executable(tinybot
                tinybot.c)
