traffic
=======

Testing framework for C programs, written in Go.

Usage
-----

Make files called `*_test.c` and run `traffic` in a folder containing them. It 
will generate, build and execute a C file that will build and run all functions 
that start with `test_` in the `_test.c` files. For verbose output, pass `-v`.

All test functions should look something like this:

```c
int foo = 7;

static char * test_foo() {
    traffic_assert("error, foo != 7", foo == 7);
    return 0;
}
```
