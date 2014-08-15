#include "maze.h"

int main(int argc, char ** argv) {
	//Clear the screen
	system("clear");
	srand(time(0));

	printf("Escape from the Everfree Forest!\n\n");
	printf("Apple Bloom is stuck in the Everfree Forest trying to get the flowers to Zecora's house.\nHelp her find her way through the scary forest! Be careful though, the Great and Powerful\nTrixie is hidden in the forest, and encountering her will surely cause your doom.\nYour flashlight lets you see one square ahead of you.\n\nMoves are given in compass directions.\nThis game is best enjoyed with headphones.\nPress Enter to start\n");
	
	getchar();

	FILE * fout = fopen(OUTPUT, APPEND);	

	int alive = false;
	int other = true;
	
	if(argc > 1) {
		populateMaze(argv[1], maze);
	} else {
		populateMaze("", maze);
	}
	fow[x][y] = true;

	fprintf(fout, "Initial position: %i,%i\n", x, y);	

	strcpy(lastMessage, "You hear Zecora's voice echo, 'Hurry up dear Apple Bloom! I need you here quick, so please come soon!'\n");

	//play the music
	system("./music | aplay 2> /dev/null 1> /dev/null &");	

	while(alive) {
		system("clear");

		printf("%s", lastMessage);
		
		printMaze(maze, fow);
		
		//Prompt for move, figure out new position
	
		printf("Your move> ");
		
		char move;

		fscanf(stdin, "%c", &move);

		int newX = x;
		int newY = y;
		
		if(move == NORTH || move == 'n') {
			newX = newX - 1;
			fow[newX-1][newY] = true;
		} else if (move == SOUTH || move == 's') {
			newX = newX + 1;
			fow[newX+1][newY] = true;
		} else if (move == EAST || move == 'e') {
			newY = newY + 1;
			fow[newX][newY+1] = true;
		} else if (move == WEST || move == 'w') {
			newY = newY - 1;
			fow[newX][newY-1] = true;
		} else if (move == 'Q' || move == 'q' || move == '\0') {
			printf("\nYou give up. You sister and her friends are stuck all kinds of messed up.\n");
			break;
		} else if (move == '\n') {
			putchar(move);
			continue;
		} else {
			printf("%x: Not a valid move", move);
		}

		printf("\n");
		
		int status = isValidMove(maze, fow, newX, newY);

		if(status != NORMAL) {
			if(status == TREEBLOCK) {
				fprintf(fout, "Moving to %i,%i - Failed, tree blocks path\n", x, y);
			} else if(status == DEAD) {
                        	fprintf(fout, "Encountered Trixie at %i,%i - it was fatal\n", x, y);
				break;
                	} else if(status == VICTORY) {
				fprintf(fout, "Found the exit at %i,%i - victory!\n", x, y);
				break;
			} 
		} else if(status == DEAD) {
			fprintf(fout, "Encountered Trixie at %i,%i - it was fatal\n", x, y);
		} else {
			fprintf(fout, "Moving to %i,%i - Succeeded\n", x, y);
		}
		
		moveHag();
	}
	
	fclose(fout);

	//kill the music process
	system("killall music aplay -s SIGKILL");	

	return 0;
}
