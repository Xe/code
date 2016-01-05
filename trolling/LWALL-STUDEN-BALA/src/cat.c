#include <stdio.h>

int main(int argc, char **argv) {
	if (argc == 2) printf(argv[1][1] == 'h' ? "hiss\n" : "");
	if (argc > 1 && argv[1][0] == '-') return 0;
	printf("MEOW!\n");

	return 0;
}
