#include <stdio.h>
#include <stdlib.h>

int main(int argc, char ** argv) {
	if(argc < 1) {
		fprintf(stderr, "Needs an argument");
		return -1;
	}

	int i = 1;

	for(; i < argc; i++) {
		fprintf(stderr, "%s ", argv[i]);
	}

	fprintf(stderr, "\n");

	return 0;
}
