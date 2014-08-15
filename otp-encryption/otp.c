#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * OTP decryptor.
 * Niichan
 * This program is public domain because I don't care.
 */

char *decrypt(char *ciphertext, char *key);

int main(int argc, char** argv) {
	FILE *keyf = fopen("key", "r");
	FILE *cipherf = fopen("cipher", "r");

	if (keyf == NULL || cipherf == NULL) {
		fprintf(stdout, "Cannot open files :(\n");
		return -1;
	}

	//Assume both files will fit in heap space
	char *key = (char *) calloc(4096, sizeof(char));
	char *cipher = (char *) calloc(4096, sizeof(char));

	//Assume the files have no spaces or newlines
	fscanf(keyf, "%s", key);
	fscanf(cipherf, "%s", cipher);
	
	printf("Message: %s\n", decrypt(cipher, key));
	
	//Free memory
	free(key);
	free(cipher);

	//Close files
	fclose(keyf);
	fclose(cipherf);

	return 0;
}

//Stolen from Parted, who is not bad at programming
char *decrypt(char *ciphertext, char *key) {
	char * decrypted = ciphertext; 
	
	while(*ciphertext) { 
		*ciphertext = ((*ciphertext - *key) % 26) + 65;
		ciphertext++; 
	}
	
	return decrypted;
}
