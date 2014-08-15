"""
Copyright (c) 2014, Jessica Williams
All rights reserved.

This software is provided 'as-is', without any express or implied
warranty. In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not
claim that you wrote the original software. If you use this software
in a product, an acknowledgment in the product documentation would be
appreciated but is not required.

2. Altered source versions must be plainly marked as such, and must not be
misrepresented as being the original software.

3. This notice may not be removed or altered from any source
distribution.
"""

class P10Line():
    def __init__(self, line):
        splitline = line.split()

        source = splitline[0]
        verb = splitline[1]

        args = splitline[2:]

        for arg in args:
            if arg.startswith(':'):
                argid = args.index(arg)
                arg = ' '.join(args[argid:])
                arg = arg[1:]
                args = args[:argid]

                args.append(arg)

                break

        self.source = source
        self.verb = verb
        self.args = args

