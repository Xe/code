#ifndef GRADES_H

#define GRADES_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX 100

int * makeAndFillArray(FILE *, int *);
FILE * openFile();
int goAgain();
double average(int, int *);
double weightedGrade(double, int, int *, int);
double gpa(double);

#endif
