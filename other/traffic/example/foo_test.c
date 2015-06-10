#include "traffic.h"

int foo = 7;
int bar = 4;

static char * test_foo() {
	traffic_assert("error, foo != 7", foo == 7);
	return 0;
}

static char * test_bar() {
	traffic_assert("error, bar != 5", bar == 5);
	return 0;
}
