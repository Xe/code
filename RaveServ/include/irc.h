#ifndef IRC_H

#define IRC_H

#include "list.h"
#include "hash.h"
#include "channel.h"

typedef struct {
	char * nick;
	char * user;
	char * host;
	char * realhost;
	char * ip;
	char * uid;
	char * login;

	long ts;
	int umodes;

	hash_t * channels;
} Client;

typedef struct {
	Client * client;
	Channel * channel;
} Source_T;

typedef struct {
	char * source;
	char * verb;
	char ** argv;
	int argc;
} IRCLine;

char * split_out(char * string, char delim);
int count_args(char * string);
IRCLine * parse_line(char * line);
void line_initialize(IRCLine * line, char * source, char * verb, int argc, char ** argv);
void line_destroy(IRCLine * line);

#endif

