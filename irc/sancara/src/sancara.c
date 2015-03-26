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

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include <unistd.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <locale.h>

#include <ncurses.h>

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include "irc.h"
#include "read_line.h"
#include "sancara.h"
#include "script.h"
#include "ui.h"

Config * conf;

void
strupper(char *s)
{
	while (*s)
	{
		if ((*s >= 'a' ) && (*s <= 'z'))
			*s -= ('a'-'A');
		s++;
	}
}

static void
panic(const char *m)
{
	endwin();
	fprintf(stderr, "Panic: %s\n", m);
	exit(1);
}

static int
dial(const char *host, short port)
{
	int f;
	struct sockaddr_in sin;
	struct addrinfo *ai, hai = { 0 };

	hai.ai_family = AF_INET;
	hai.ai_socktype = SOCK_STREAM;
	if (getaddrinfo(host, 0, &hai, &ai))
		panic("Cannot resolve host.");
	memcpy(&sin, ai->ai_addr, sizeof sin);
	sin.sin_port = htons(port);
	freeaddrinfo(ai);
	f = socket(AF_INET, SOCK_STREAM, 0);
	if (f<0)
		panic("Cannot create socket.");
	if (connect(f, (struct sockaddr *)&sin, sizeof sin)<0)
		panic("Cannot connect to host.");
	return f;
}

void
send_line(int fd, char * line) {
	write(fd, line, strlen(line));
	write(fd, "\r\n", 2);
}

void send_linef(int fd, char * format, ...) {
	char buf[513];
	va_list ap;
	va_start(ap, format);

	vsnprintf(buf, 512, format, ap);
	send_line(fd, buf);
}

int
main(int argc, char ** argv)
{
	int fd, o;

	char * sbuf;
	char * prompt, * topic, * status;
	WINDOW * client;
	IRCNetwork * net;

	init_curses();
	init_lua();

	conf = malloc(sizeof(Config));
	read_config(conf);
	net = malloc(sizeof(IRCNetwork));

	client = newwin(LINES-3, COLS, 1, 0);
	wrefresh(client);
	scrollok(client, TRUE);

	sbuf = malloc(513*sizeof(char));

	draw_topicbar("connecting...\n");
	wprintw(client, "Connecting to %s", conf->host);
	refresh();

	net->fd = dial(conf->host, conf->port);
	net->host = conf->host;
	net->port = conf->port;

	draw_topicbar("Socket open");
	refresh();

	send_linef(net->fd, "NICK %s", conf->nick);
	send_linef(net->fd, "USER %s 0 0 :%s", conf->user, conf->real);

	prompt = "(status) ";

	while((o = readLine(net->fd, sbuf, 512)))
	{
		sbuf[o] = '\0';

		IRCLine * line;
		line = parse_line(sbuf);

		if(strncmp(sbuf, "PING", 4) == 0)
		{
			sbuf[1] = 'O';
			send_line(net->fd, sbuf);
		}
		else if (!strncmp(sbuf, "ERROR", 5)) {
			panic(sbuf);
		}

		if(!strncmp(line->verb, "001", 3)) { // RPL_WELCOME
			send_line(net->fd, "JOIN #!");
			wprintw(client, "\n%s", line->argv[1]);
		} else if(!strncmp(line->verb, "002", 3)) { // RPL_MYINFO
			wprintw(client, "\n%s", line->argv[1]);
		} else if(!strncmp(line->verb, "003", 3)) { // RPL_CREATED
			wprintw(client, "\n%s", line->argv[1]);
		} else if(!strncmp(line->verb, "004", 3)) {
			wprintw(client, "\nIRCD version: %s, User modes: %s, Channel modes: %s, Parametric modes: %s",
				line->argv[2], line->argv[3], line->argv[4], line->argv[5]);
		} else if(!strncmp(line->verb, "332", 3)) { // Topic for channel
			wprintw(client, "\nTopic for %s: %s", line->argv[1],
					line->argv[2]);
			topic = line->argv[2];
			draw_topicbar(line->argv[2]);
		} else if(!strncmp(line->verb, "333", 3)) { // Topic set by
			wprintw(client, "\nTopic set by %s at %s",
					strtok(line->argv[2], "!"), line->argv[3]);
		} else if(!strncmp(line->verb, "366", 3)) { // End of /names
			// Ignore this numeric
		} else if(!strncmp(line->verb, "372", 3)) { // MOTD content
			wprintw(client, "\n = %s", line->argv[1]);
		} else if(!strncmp(line->verb, "376", 3)) { // End of MOTD
			wprintw(client, "\n = End of MOTD for %s", line->source);
		} else if(!strncmp(line->verb, "JOIN", 4)) {
			char *source = strtok(line->source, "!");
			wprintw(client, "\n--> %s joined %s", line->source, line->argv[0]);
			draw_prompt(prompt);
		} else if(!strncmp(line->verb, "NOTICE", 6)) {
			wprintw(client, "\n-%s- %s", line->source, line->argv[1]);
		} else if(!strncmp(line->verb, "PRIVMSG", 7)) {
			char * source = strtok(line->source, "!");
			wprintw(client, "\n<%s> %s", line->source, line->argv[1]);
		} else if(!strncmp(line->verb, "QUIT", 4)) {
			char * source = strtok(line->source, "!");
			wprintw(client, "\n<-- %s has quit (%s)",
					line->source, line->argv[0]);
		} else if(!strncmp(line->verb, "PART", 4)) {
			char * source = strtok(line->source, "!");
			wprintw(client, "\n<-- %s has left %s (%s)",
					line->source, line->argv[0], line->argv[1]);
		} /* else {
			wprintw(client, "\n<> line->source: %s line->verb: %s, line->argc: %d, line->args: [ ",
					line->source, line->verb, line->argc);
			for(i = 0; i < line->argc; i++) {
				wprintw(client, "\"%s\" ", line->argv[i]);
			}
			wprintw(client, "]");
		} */
		wrefresh(client);

		draw_statusbar(sbuf);
		draw_prompt(prompt);

		line_destroy(line);
	}
	free(sbuf);
	free(conf);
	free(net);
	getch();
	delwin(client);

	destroy_curses();
	destroy_lua();

	return 0;
}
