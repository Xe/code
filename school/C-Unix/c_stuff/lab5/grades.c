#include "grades.h"

int * makeAndFillArray(FILE * fin, int * count) {
	int total;

	fscanf(fin, "%i", &total);

	*count = total;

	//printf("%d\n", total);

	int * ara = (int *) malloc(*count * sizeof(int));
	
	//printf("%s\n", ara == NULL ? "YES" : "NO");

	int i, * ptr = ara;

	for(i = 0; i < *count; i++) {
		fscanf(fin, "%d", ptr);
		ptr++;
		//printf("%i\n", ara[i]);
	}

	//ara -= count-1;

	return ara;
}

FILE * openFile() {
	FILE * fin = NULL;

	do {
		char fname[MAX];
		fprintf(stderr, "fname> ");
		fscanf(stdin, "%s", fname);
		fin = fopen(fname, "r");

		if(fin == NULL) {
			fprintf(stderr, "name %s rejected, check permissions and retry\n", fname);
		}
	} while (fin == NULL);

	return fin;
}

int goAgain() {
	char userInput;

	getchar();
	//fflush(stdin);

	printf("Again? (y/n): ");
	scanf("%c", &userInput);

	return userInput == 'y';
}

double average(int count, int * array) {
	double val = 0.0;
	int i = 0, * ptr = array;

	for (i = 0; i < count; i++) {
		val = val + *ptr;
		ptr++;
	}
	
	val = val / count;
	
	return val;
}

double weightedGrade(double weight, int count, int * ara, int max) {
	int i, r = 0, * ptr = ara;
	
	for (i = 0; i < count; i++) {
		r += *ptr;
		ptr++;
	}
	
	//typecast here just to make sure the types are correct
	return ((double)r / (count * max)) * weight;
}

double gpa(double perc) {
	if(perc > .949) {
		return 4.0;
	} else if (perc < .60) {
		return 0.0;
	} else {
		return 3.9 - 10*(.94 - perc);
	}
}
