#include "grades.h"

/* a small main you will need to add to. You can't change my function calls */

int main()
{
	FILE * fin = NULL;
	int * quizzes, * assignments, * exams, * finals;
	int quizTotal, assignTotal, examTotal, finalTotal;
	char name[MAX];

	do
	{
		fin = openFile();

		fgets(name, MAX, fin);

		assignments = makeAndFillArray(fin, &assignTotal);
		quizzes = makeAndFillArray(fin, &quizTotal);
		exams = makeAndFillArray(fin, &examTotal);
		finals = makeAndFillArray(fin, &finalTotal);

		fclose(fin);
		
		double assgnAvg = average(assignTotal, assignments);
		double quizAvg = average(quizTotal, quizzes);
		double examAvg = average(examTotal, exams);
		double finalAvg = average(finalTotal, finals);
		
		//derp
		
		printf("name: %s\n", name);
		printf("Assignment Avg:\t\t%f\n", assgnAvg);
		printf("Quiz Avg:\t\t%f\nExam Avg:\t\t%f\nFinal Avg:\t\t%f\n", quizAvg, examAvg, finalAvg);

		double rawScore = weightedGrade(.4, assignTotal, assignments, 100) + weightedGrade(.1, quizTotal, quizzes, 25) + weightedGrade(.25, examTotal, exams, 100) + weightedGrade(.25, finalTotal, finals, 200);
		printf("Raw Score:\t\t%f\n", rawScore);

		printf("GPA:\t\t%3.2f\n", gpa(rawScore));

		free(assignments);
		free(quizzes);
		free(exams);
		free(finals);

	}while(goAgain());
		

	return 0;

}// end main

