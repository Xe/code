#include "maze.h"

//extra credit possible on this assignment if you make the hag move

int x = 0, y = 0, hagx = 0, hagy = 0;

void printMaze(char maze[][COLS], int fow[][COLS]) {
	int i = 0, j = 0;

	//system("clear");
	
	//can omit initilization stanza here
	for(; i < ROWS; i++) {
		for(j = 0; j < COLS; j++) {
			if(!fow[i][j]) {
				if(i == x && j == y) {
                         		putchar(DUDEMAN);
                        	} else if(maze[i][j] == START) {
					putchar(START);
				} else if(hagx == i && hagy == j) {
					putchar(HAG);
				} else {
					putchar(maze[i][j]);
				}
			} else {
				putchar(UNSHOWN);
			}
			
			putchar(' ');
		}
		
		putchar('\n');
	}
}

int isValidMove(char grid[][COLS], int fow[][COLS], int nextx, int nexty) {
	if(nextx > ROWS-1) {
		nextx = ROWS-1;
	} else if (nextx < 0) {
		nextx = 0;
	}

	if(nexty > COLS-1) {
		nexty = COLS-1;
	} else if (nexty < 0) {
		nexty = 0;
	} 

	char mapVal = grid[nextx][nexty];
		
	if(mapVal == TREE) { 
		//I would like to be a tree, but cannot :(
		strcpy(lastMessage, "Trees do not let you pass!\n");
		fow[nextx][nexty] = true;
		return TREEBLOCK;
	} else if(nextx == hagx && nexty == hagy) {
		strcpy(lastMessage, "You ran into The Great and Powerful Trixie. You are ignorant enough to challenge her\nmagical prowess. You are dead.\n");
		printf(lastMessage);
		return DEAD;
	} else if(mapVal == FINISH) {
		strcpy(lastMessage, "Woohoo! You found Zecora's hut!\nNow you can get the potion ready to save your sister!\n");
		printf(lastMessage);
		return VICTORY;
	} else {
		x = nextx;
		y = nexty;
		
		fow[nextx][nexty] = true;

		strcpy(lastMessage, "\n");			

		return NORMAL;
	}
}

void populateMaze(char * name, char maze[][COLS]) {
	FILE * fin;
	fin = fopen(name, "r");

	//open the file	

	int firstTry  = false;	

	if (fin == NULL) {
		/*do {
			char fname[FNAME];

			if(!firstTry) {
				fprintf(stderr, "Bad file name %s\n", name);
			} else {
				firstTry = false;
			}
	
			fprintf(stdout, "fname> ");
					
			fscanf(stdin, "%s", fname);
			
			//while(fgetc(stdin) != '\n');
			
			fin = fopen(name, "r");
		} while (fin == NULL);*/
		
		do {
			char fname[FNAME];
                	fprintf(stderr, "fname> ");
                	fscanf(stdin, "%s", fname);
                	fin = fopen(fname, "r");

                	if(fin == NULL) {
                        	fprintf(stderr, "name %s rejected, check permissions and retry\n", fname);
                	}
        	} while (fin == NULL);
	}

	//fill the maze and locate the starting point

	int i, j;
	
	for (i = 0; i < ROWS; i++) {
		for (j = 0; j < COLS; j++) {
			fscanf(fin, "%s", &maze[i][j]);
			//fscanf(fin, "%c");
			
			if(maze[i][j] == START) {
				x = i;
				y = j;
			} else if(maze[i][j] == HAG) {
				hagx = i;
				hagy = j;
				
				maze[i][j] = OPEN;
			}
			
			fow[i][j] = false;
			//fow[i][j] = true;
		}
	}
	
	fow[x][y] = true;
}

void moveHag() {
	int temp = rand(), canMove = false, movingdirection = 0;

	//canMove = true;
	movingdirection = rand() % 4;
	
	if(canMove) {
		int newX = hagx, newY = hagy;
		if(movingdirection == 0) {
			//move north
			newX -= (rand() % 4);
		} else if (movingdirection == 1) {
			//move east
			newY -= (rand() % 4);
		} else if (movingdirection == 2) {
			//move south
			newX += (rand() % 4);
		} else if (movingdirection == 3) {
			//move west
			newY += (rand() % 4);
		}
		
		if (newX < 0) {
			newX = hagx;
		} else if (newX > ROWS-1) {
			newX = hagx;
		} 
		
		if (newY < 0) {
			newY = hagy;
		} else if (newY > COLS-1) {
			newY = hagy;
		}

		if(maze[newX][newY] == TREE) {
			temp = rand();
			newX = temp % ROWS;
			temp = rand();
			newY = temp % COLS;
		}

		fow[newX][newY] = false; //Make squares not visible
		
		hagx = newX;
		hagy = newY;
	}
}
