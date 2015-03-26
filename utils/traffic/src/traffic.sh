#!/bin/bash

DIR=$(mktemp -d)
TEST_FILES=$(ls *_test.c | sed ':a;N;$!ba;s/\n/ /g')

echo '#define traffic_assert(message, test) do { if (!(test)) return message; } while (0)
#define traffic_run_test(test) do { char *message = test(); tests_run++; if (message) return message; } while (0)
extern int tests_run;' > $DIR/traffic.h

echo '#include <stdio.h>
#include "traffic.h"

int tests_run = 0;' >> $DIR/main.c

for file in $TEST_FILES
do
	echo '#include "'"traffic-$(echo $file | sed 's/.c/.h/')"'"' >> $DIR/main.c
done

echo 'static char * all_tests() {' >> $DIR/main.c

for file in $TEST_FILES
do
	for func in $(grep "static char \* test_" $file | cut -d' ' -f4 | tr -d '()')
	do
		echo "	traffic_run_test($func);" >> $DIR/main.c
	done

	grep "static char \* test_" $file | tr '{' ';' > $DIR/"traffic-$(echo $file | sed 's/.c/.h/')"
done

echo '	return 0;
}' >> $DIR/main.c

echo 'int main(int argc, char **argv) {
    char *result = all_tests();
    if (result != 0) {
        printf("%s\n", result);
    }
    else {
        printf("ALL TESTS PASSED\n");
    }
    printf("Tests run: %d\n", tests_run);

    return result != 0;
}' >> $DIR/main.c

gcc -I $DIR -o $DIR/test.out $DIR/main.c $TEST_FILES

~/bin/ptar c $DIR

rm -rf $DIR
