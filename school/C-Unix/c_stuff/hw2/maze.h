#ifndef MAZE_H

#define MAZE_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <time.h>

#define ROWS 20
#define COLS 25
#define FNAME 256

#define true 0
#define false 1

//Into definitions for the characters in this game
#define TREE   	'T'
#define OPEN   	'.'
#define UNSHOWN 'U'
#define HAG   	'H'
#define START  	'S'
#define FINISH  'F'
#define NORTH	'N'
#define SOUTH	'S'
#define WEST	'W'
#define EAST	'E'
#define QUIT	'Q'
#define DUDEMAN	'@'
#define APPEND	"a"
#define OUTPUT	"output.txt"

#define DEAD	666
#define VICTORY	42
#define NORMAL	8
#define TREEBLOCK 9

//variables that we need
int x,y, hagx, hagy;
char fname_in[FNAME];
char fname_out[FNAME];
char lastMessage[FNAME];
char maze[ROWS][COLS];
int fow[ROWS][COLS];
//Don't let this int type declaration decieve you, this is going to be treated as a bunch of boolean values.

void printMaze(char[][COLS], int[][COLS]);
int isValidMove(char[][COLS], int[][COLS], int, int);
void populateMaze(char *, char[][COLS]);
void moveHag();

#endif
