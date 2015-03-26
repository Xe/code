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

#include <ncurses.h>
#include <stdlib.h>

#include "ui.h"

void init_curses() {
	initscr();

	cbreak();
	echo();
	noraw();
	keypad(stdscr, TRUE);

	if (has_colors() == FALSE) {
		destroy_curses();
		printf("You need a terminal with colors.\n\n");
		exit(1);
	};

	start_color();

	init_pair(1, COLOR_WHITE, COLOR_GREEN);
}

void destroy_curses() {
	endwin();
}

void draw_topicbar(char * topic) {
	move(0,0);
	clrtoeol();

	printw(topic);
	mvchgat(0, 0, -1, 0, 1, NULL);
}

void draw_statusbar(char * content) {
	int row, col;

	getmaxyx(stdscr, row, col);

	move(row-2, 0);
	clrtoeol();
	printw(content);
	mvchgat(row-2, 0, -1, 0, 1, NULL);

	refresh();
}

void draw_prompt(char * prompt) {
	int row, col;

	getmaxyx(stdscr, row, col);

	move(row-1, 0);
	clrtoeol();
	printw(prompt);

	refresh();
}

