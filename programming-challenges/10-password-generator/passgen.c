#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>

#define CHARSET_MASK_LETTERS 2
#define CHARSET_MASK_NUMBERS 4
#define CHARSET_MASK_SYMBOLS 8

char *charset_letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
char *charset_numbers = "0123456789";
char *charset_symbols = "!@#$%^&*()";

void gen_password(int len, char *charset, char *output);

void usage() {
	printf("passgen - A simple password generator.\n");
	printf("Written by AppleDash\n");
	printf("Options:\n");
	printf("\t-h: Print this help message and exit.\n");
	printf("\t-l <length>: Specify the length of the password to generate. (Required)\n");
	printf("\nOne or more of the following options are required:\n");
	printf("\t-L: Include letters ([a-zA-z]) in the charset for password generation.\n");
	printf("\t-N: Include numbers ([0-9]) in the charset for password generation.\n");
	printf("\t-S: Include symbols (the ones on the number keys) in the charset for password generation.\n");
}

int main(int argc, char *argv[]) {
	char opt;
	int passlen, len;
	char *password;
	char *charset;
	int count;
	int charset_mask = 0;
	extern char *optarg;

	while ((opt = getopt(argc, argv, "hl:LNS")) != -1) {
		switch (opt) {
			case 'h':
				usage();
				return 0;
			case 'l':
				passlen = atoi(optarg);
				break;
			case 'L':
				charset_mask |= CHARSET_MASK_LETTERS;
				break;
			case 'N':
				charset_mask |= CHARSET_MASK_NUMBERS;
				break;
			case 'S':
				charset_mask |= CHARSET_MASK_SYMBOLS;
				break;
		}
	}

	if (passlen < 1) {
		printf("Error: Password length missing or less than one.\n");
		return 1;
	}

	if (charset_mask == 0) {
		printf("Error: No charset options specified.\n");
		return 1;
	}

	len = strlen(charset_letters) + strlen(charset_numbers) + strlen(charset_symbols) + 1;
	charset = (char *)malloc(len);
	memset(charset, 0, len);

	count = 0;

	if (charset_mask & CHARSET_MASK_LETTERS) {
		memcpy(charset, charset_letters, strlen(charset_letters));
		count += strlen(charset_letters);
	}

	if (charset_mask & CHARSET_MASK_NUMBERS) {
		memcpy(charset + count, charset_numbers, strlen(charset_numbers));
		count += strlen(charset_numbers);
	}

	if (charset_mask & CHARSET_MASK_SYMBOLS) {
		memcpy(charset + count, charset_symbols, strlen(charset_symbols));
	}

	
	password = malloc(passlen+1);
	memset(password, 0, passlen+1);	
	gen_password(passlen, charset, password);
	printf("%s\n", password);

	free(charset);
	free(password);

	return 0;
}

int rand_range(int lower, int upper) {
	return (lower + rand() / (RAND_MAX / (upper - lower + 1) + 1));
}

/* Seed rand() from /dev/random */
void seed_random() {
	FILE *fp;
	unsigned int seed;

	fp = fopen("/dev/random", "r");
	fread(&seed, sizeof(unsigned int), 1, fp);
	fclose(fp);
	srand(seed);
}

void gen_password(int len, char *charset, char *output) {
	int charset_len = strlen(charset);
	int i;
	
	seed_random();

	for (i = 0; i < len; i++) {
		int index = rand_range(0, charset_len-1);
		output[i] = charset[index];
	}
}