#!/usr/bin/env python2
from sys import argv, exit

if __name__ == "__main__":
    if len(argv) != 2:
        print("Usage: %s \"string to reverse\"" % argv[0])
        exit(1)

    reversed = argv[1][::-1]  # Use slicing to reverse the string
    print(reversed)
