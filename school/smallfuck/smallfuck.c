#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXPROG 2048
#define TAPEMAX 2048
#define UNHAPPY -1

int main (int argc, char ** argv) {
	if(argc < 2) {
		fprintf(stderr, "%s usage summary:\n\n", argv[0]);
		fprintf(stderr, "%s <file to read from>\n\n", argv[0]);

		return UNHAPPY;
	}
	
	char * program = (char *) calloc(MAXPROG, sizeof(char));
	char * tape = (char *) calloc(TAPEMAX, sizeof(char));

	//printf("%s\n", program == NULL ? "Bad alloc" : "good alloc");

	FILE * fin = NULL;

	fin = fopen(argv[1], "r");

	if (fin == NULL) {
		fprintf(stderr, "File %s could not be opened, exiting uncleanly\n", argv[1]);
		return UNHAPPY;
	}

	int count = 0;

	//parse the program and load it
	while(!feof(fin)) {
		char temp = fgetc(fin);

		program[count] = temp;
		count++;
	}

	//program[count] = '\0';

	int len = strlen(program);

	//printf("Program length is %i instructions\n", len);
	//printf("Program: %s\n", program);

	int i = 0;
	int * ptr = (int *) tape;

	for(i = 0; i < len-1; i++) {
		if (program[i] == '>') {
			++ptr;
		} else if (program[i] == '<') {
			--ptr;
		} else if (program[i] == '+') {
			*ptr++;
		} else if (program[i] == '-') {
			*ptr--;
		} else if (program[i] == '.') {
			putchar(*ptr);
		} else if (program[i] == ',') {
			//*ptr = getchar();
			char temp = getchar();
			if(temp == '\n') {
				*ptr = getchar();
			} else {
				*ptr = temp;
			}
		} 

		printf("Memory: %s\n", tape);
	}

	free(program);
	free(tape);

	return 0;
}
