#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ASSGN 7
#define QUIZ  8
#define EXAMS 2
#define FINAL 1
#define NAME  100

//Why can't I hold all these functions?
//[](/ppseesyou)
void fillArray(int, int[]);
double average(int, int[]);
double weightedGrade(double, int, int[], int);
double toPercent(double, int);
double gpa(double);

int main(int argc, char** argv) {
	int assignments[ASSGN];
	int quizzes[QUIZ];
	int exams[EXAMS];
	int final[FINAL];
	char name[NAME];

	fgets(name, NAME, stdin);

	fillArray(ASSGN, assignments);
	fillArray(QUIZ, quizzes);
	fillArray(EXAMS, exams);
	fillArray(FINAL, final);
	
	printf("Welcome to the Grade Calculator Program.\n\nThis program will produce a weighted percentage based on the scores earned in four\ncategories: assignments, quizzes, exams, and a final exam. In addition to the weighted\npercentage, the associated Grade Point will also be reported.\n\n");

	printf("Reporting on: %s\n", name);

	printf("Averages:\n");
	printf("Avg of assignments:\t%3.0f%%\n", average(ASSGN, assignments));
	printf("Avg of quizzes:\t\t%3.0f%%\n", average(QUIZ, quizzes)*4);
	printf("Avg of exams:\t\t%3.0f%%\n", average(EXAMS, exams));
	printf("Final score:\t\t%3.0f%%\n", (double)final[0]/2);

	printf("\nWeighted grades:\n");
	double rawScore = weightedGrade(.4, ASSGN, assignments, 100) + weightedGrade(.1, QUIZ, quizzes, 25) + weightedGrade(.25, EXAMS, exams, 100) + weightedGrade(.25, FINAL, final, 200);
	printf("total:\t\t\t%3.2f\n", rawScore);
	printf("GPA:\t\t\t%3.2f\n\n", gpa(rawScore));
	
	return 0;
}

void fillArray(int count, int array[]) {
	int i;

	for (i = 0; i < count; i++) {
		//Get all the values in, it should only be ints by this point
		fscanf(stdin, "%i", &array[i]);
	}
}

//reduces and averages an array of ints as a double
double average(int count, int array[]) {
	double val = 0.0;
	int i = 0;	

	for (i = 0; i < count; i++) {
		val = val + array[i];
	}

	val = val / count;
	
	return val;
}

//Calculates the weighting of the grades
double weightedGrade(double weight, int count, int ara[], int max) {
	int i, r = 0;
	
	for (i = 0; i < count; i++) {
		r += ara[i];
	}
	
	//typecast here just to make sure the types are correct
	return ((double)r / (count * max)) * weight;
}

//Calculates the GPA based on the percentage and the formula given to us
double gpa(double perc) {
	if(perc > .949) {
		return 4.0;
	} else if (perc < .60) {
		return 0.0;
	} else {
		return 3.9 - 10*(.94 - perc);
	}
}