#include <stdio.h>
#include <stdlib.h>

int main(int argc, char ** argv) {
	int i = 1;

	for(; i < argc; i++) {
		fprintf(stderr, "%s ", argv[i]);
	}

	fprintf(stderr, "\n");
}
