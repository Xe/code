#include <stdio.h>

#define DAYS_IN_YEAR 365
#define HOURS_IN_DAY 24
#define MINUTES_IN_HOUR 60
#define SECONDS_IN_MINUTE 60

int main(int argc, char *argv[]) {
	int age_in_years;
	unsigned int age_in_seconds;

	if (argc != 2) {
		printf("Usage: %s <age in years>\n", argv[0]);
		return 1;
	}

	age_in_years = atoi(argv[1]);

	if (age_in_years < 1) {
		printf("Error: Age in years must be greater than zero!\n");
		return 1;
	}

	age_in_seconds = age_in_years * DAYS_IN_YEAR * HOURS_IN_DAY * MINUTES_IN_HOUR * SECONDS_IN_MINUTE;
	printf("%d\n", age_in_seconds);

	return 0;
}