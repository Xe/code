#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Christine Dodrill
 * CSCD 240
 * /dev/null is a bad input file
 */

#define ROW 10
#define COL ROW
#define DLEN 16
#define MODE "r"
#define FILENAME "input.txt"
#define false 1
#define true 0

//Posix standards don't like filenames over 256 chars
#define FNAME 256

void printChar(int);
void fillArray(int, int*, FILE*);
void fillGrid(int[][COL], FILE*);
void printGrid(int[][COL]);
void gridcpy(int[][COL], int[][COL]);
void printSolution(int*);
void conway(int[][COL], int[][COL], int*);

void main(int argc, char** argv) {
	char fname[FNAME]; 
	FILE * fin = NULL;
	int iterations = 0;

	//Grab input file from command line if it is given,
	//or just use the hard-coded defined FILENAME
	/*if(argc > 1) {
		strcpy(fname, argv[1]);
	} else {
		strcpy(fname, FILENAME);
	}*/

//	fin = fopen(fname, MODE);
			
	do {
		fprintf(stderr, "fname> ");
		fscanf(stdin, "%s", fname);
		fin = fopen(fname, MODE);
		
		if(fin == NULL) {
			fprintf(stderr, "name %s rejected, check permissions and retry\n", fname);
		}
	} while (fin == NULL);

/*	if(fin == NULL) {
		fprintf(stderr, "the filesystem rejected %s, using \"input.txt\"\n", fname);
		fin = fopen(FILENAME, MODE);
		
		if(fin == NULL) {
			fprintf(stderr, "fname> ");
			fscanf(stdin, "%s", fname);
			fin = fopen(fname, MODE);
			
			if(fin == NULL) {
				fprintf(stderr, "You fail at life.\n");
				return;
			}
		}
	}*/

	//Read the iterations count from fin
	fscanf(fin, "%i", &iterations);
	printf("%i iterations\n", iterations);

	int i;
	for (i = 0; i < iterations; i++) {
		int days;
		int solution[DLEN];
		int grid[ROW][COL];
		
		fscanf(fin, "%i", &days);
		printf("Days to run: %i\n", days);
		
		fillArray(DLEN, solution, fin);
		printSolution(solution);

		fillGrid(grid, fin);
		
		int j = 0;
		
		for(;j < days+1; j++) {
			printf("Generation %i of sample %i:\n", j, i+1);
			
			printGrid(grid);
			
			printf("\n");
			
			int nextGrid[ROW][COL];
			
			conway(grid, nextGrid, solution);
			gridcpy(nextGrid, grid);
		}
		
		printf("\n");
	}
}

void printChar(int val) {
	switch (val) {
		default : 
			printf("."); 
			break;
		case 1 : 
			printf("!"); 
			break;
		case 2 : 
			printf("X"); 
			break;
		case 3 : 
			printf("#"); 
			break;
	}
}

void fillArray(int count, int * ara, FILE * fin) {
	int i;
	
	for (i = 0; i < count; i++) {
		fscanf(fin, "%i", &ara[i]);
	}
}

void fillGrid(int grid[][COL], FILE * fin) {
	int i,j;
	
	for (i = 0; i < ROW; i++) {
		for (j = 0; j < COL; j++) {
			fscanf(fin, "%i", &grid[i][j]);
		}
	}
}

void gridcpy(int a[][COL], int b[][COL]) {
	int i,j;
	
	for (i = 0; i < ROW; i++) {
		for (j = 0; j < COL; j++) {
			b[i][j] = a[i][j];
		}
	}
}

void printSolution(int * solution) {
	int i = 0;
	
	printf("solution: (");
	
	for(i = 0; i < DLEN; i++) {
		printf("%d", solution[i]);
		if(i!=DLEN-1) {
			printf(" ");
		}
	}
	
	printf(")\n");
}

void printGrid(int grid[][COL]) {
	int i,j;
	
	for(i = 0; i < ROW; i++) {
		for(j = 0; j < COL; j++) {
			printChar(grid[i][j]);
		}
		
		printf("\n");
	}
}

void conway(int grid[][COL], int nextGrid[][COL], int* solution) {
	int i = 0, j = 0;
	for(i = 0; i < ROW; i++) {
		for(j = 0; j < COL; j++) {
			int score = grid[i][j];
			
			if(j == 0) {
				score += grid[i][j+1]; //one col right
			} else if (j == ROW-1) {
				score += grid[i][j-1]; //one col left
			} else {
				score += grid[i][j-1]; //one col left
				score += grid[i][j+1]; //one col right
			}
			
			if (i == COL-1) {
				score += grid[i-1][j]; //one row up
			} else if (i == 0) {
				score += grid[i+1][j]; //one row down
			} else {
				score += grid[i-1][j]; //one row up
				score += grid[i+1][j]; //one row down
			}
			
			int valToCommit = solution[score] + grid[i][j];
			
			if(valToCommit > 3) {
				valToCommit = 3;
			} else if (valToCommit < 0) {
				valToCommit = 0;
			}
			
			nextGrid[i][j] = valToCommit;
		}
	}
}
