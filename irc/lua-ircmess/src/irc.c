#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "irc.h"

char * split_out(char * string, char delim)
{
	int i;

	for(i = 0; strlen(string) && string[i] != delim; i++);

	return string + i + 1;
}

int count_args(char * string)
{
	unsigned int i;
	int argc = 0;

	for(i = 0; i < strlen(string); i++) {
		if(string[i] == ' ')
			argc++;
		if(string[i+1] == ':' && string[i] == ' ')
			break;
	}

	return argc;
}

IRCLine * parse_line(char * line)
{
	char * parsed, * source, * extparam, * verb;
	int argc, i;

	IRCLine * result;

	if(line[strlen(line)-2] == '\r')
		line[strlen(line)-2] = '\0';

	extparam = parsed = calloc(sizeof(char), strlen(line) + 1);
	strncpy(parsed, line, strlen(line) + 1);

	result = malloc(sizeof(IRCLine));

	argc = count_args(line);
	char * argv[argc];

	if(line[0] == ':') {
		argc--;
		line++;
		parsed++;
		source = strtok(parsed, " ");
		verb = strtok(NULL, " ");
	} else {
		source = "";
		verb = strtok(parsed, " ");
	}

	for(i = 0; i < argc; i++) {
		argv[i] = NULL;
		argv[i] = strtok(NULL, " ");
	}

	if(*argv[argc-1] == ':') {
		argv[argc-1] = split_out(line, ':');
	}

	line_initialize(result, source, verb, argc, argv);

	free(extparam);

	return result;
}

void line_initialize(IRCLine * line, char * source, char * verb, int argc, char ** argv)
{
	int i;

	line->source = calloc(strlen(source)+1, sizeof(char));
	strncpy(line->source, source, strlen(source));

	line->verb = calloc(strlen(verb)+1, sizeof(char));
	strncpy(line->verb, verb, strlen(verb));

	line->argc = argc;

	line->argv = calloc(argc, sizeof(char*));

	for(i = 0; i < argc; i++) {
		line->argv[i] = calloc(strlen(argv[i]+1), sizeof(char));
		strncpy(line->argv[i], argv[i], strlen(argv[i]));
	}
}

void line_destroy(IRCLine * line)
{
	int i;

	free(line->source);
	free(line->verb);

	for(i = 0; i < line->argc; i++) {
		free(line->argv[i]);
	}

	free(line->argv);

	free(line);
}

