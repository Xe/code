#ifndef IRC_H

#define IRC_H

typedef struct {
	char * nick;
	char * user;
	char * host;
	int is_server;
} Source;

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

