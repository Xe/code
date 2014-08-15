# Copyright (C) 2014 Sam Dodrill <xena@yolo-swag.com> All rights reserved.
#
# This software is provided 'as-is', without any express or implied
# warranty. In no event will the authors be held liable for any damages
# arising from the use of this software.
#
# Permission is granted to anyone to use this software for any purpose,
# including commercial applications, and to alter it and redistribute it
# freely, subject to the following restrictions:
#
# 1. The origin of this software must not be misrepresented; you must not
#    claim that you wrote the original software. If you use this software
#    in a product, an acknowledgment in the product documentation would be
#    appreciated but is not required.
#
# 2. Altered source versions must be plainly marked as such, and must not be
#    misrepresented as being the original software.
#
# 3. This notice may not be removed or altered from any source
#    distribution.
#

import os
import sys

def is_upper(character):
    """
    Returns True if the character is upper case and False if it is not a
    single character (a string with a length longer than one) or it is lower
    case.
    """
    try:
        assert len(character) == 1
    except:
        return False

    val = ord(character)

    if val >= 65 and val <= 90:
        return True
    else:
        return False

def flatten_string(string):
    """
    Performs transformations such as:

        ToString -> to_string

    This is useful when converting GoStyleNaming to json_and_python_style_naming.
    """
    newstring = ""
    first = True

    for character in string:
        if is_upper(character):
            if first:
                first = False
            else:
                newstring += "_"

        newstring += character.lower()

    return newstring

if __name__ == "__main__":
    lines = None
    outlines = []

    if len(sys.argv) != 2:
        print """Usage: %s <path to file>

This will convert go structs to use JSON naming. Do not use this on a file that
has anything but Go structs in it."""
        sys.exit(1)

    # Read in the file to be modified
    with open(sys.argv[1], "r") as fin:
        lines = fin.read().split("\n")

    for line in lines:
        split = line.split()

        try:
            # HACK: check if we need to care about the line
            assert len(split) == 2
        except:
            outlines.append(line)
            continue

        name = split[0]

        # HACK: "package confgen" and "\tThingName type" will match, detect this.
        if len(split) == 2 and name != "package":
            flattened = flatten_string(name)
            outlines.append("\t%s %s `json:\"%s\"`" % (name, split[1], flattened))
        else:
            outlines.append(line)

    with open(sys.argv[1], "w") as fout:
        for line in outlines:
            fout.write("%s\n" % line)

    # Run this through gofmt...results are very ugly otherwise.
    os.system("gofmt %s" % sys.argv[1])

    print "%s formatted for json output" % sys.argv[1]

