#!/usr/bin/env python2
from sys import argv, exit

if __name__ == "__main__":
    if len(argv) != 2:
        print("Usage: %s \"string to count words in\"" % argv[0]);
	exit(1)

    words = argv[1].split()  # Split on all whitespace, not just spaces.
    print(len(words))
