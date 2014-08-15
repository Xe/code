"""
Copyright (c) 2013, Kiyoshi Aman

Permission to use, copy, modify, and/or distribute this software for any purpose
with or without fee is hereby granted, provided that the above copyright notice
and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.
"""

class IRCMessage:
    def __init__(self, data):
        self._parse(data)

    def _parse(self, input):
        """Parse an IRC message.

        >>> parse(':lol!lol@example.com JOIN #lol')
        ({}, u':lol!lol@example.com', u'JOIN', [u'lol'])
        >>> parse('@foo=bar :lol!lol@example.com PRIVMSG #lol :lol')
        ({u'@foo': u'bar'}, u':lol!lol@example.com', u'PRIVMSG', [u'#lol', u':lol'])
        """
        if isinstance(input, bytes):
            input = input.decode('UTF-8', 'replace')

        string = input.split(' ')

        if string[0].startswith('@'):
            tag_str = string[0][1:].split(',')
            string = string[1:]
            tags = {}

            for tag in tag_str:
                k, v = tag.split('=', 1)
                tags[k] = v
        else:
            tags = {}

        if string[0].startswith(':'):
            prefix = string[0][1:]
            string = string[1:]
        else:
            prefix = None

        verb = string[0].upper()
        args = string[1:]

        for arg in args:
            if arg.startswith(':'):
                idx  = args.index(arg)
                arg  = ' '.join(args[idx:])
                arg  = arg[1:]
                args = args[:idx]

                args.append(arg)

                break

        self.tags = tags
        self.source = prefix
        self.verb = verb
        self.args = args

    def args_str(self):
        base = []
        for arg in self.args:
            if ' ' not in arg:
                base.append(arg)
            else:
                base.append(':%s' % (arg))
                break

        return ' '.join(base)

    def __str__(self):
        if self.tags:
            tags = ['%s=%s' % (k, v) for k, v in self.tags.items()]
            tags = '@%s ' % (','.join(tags))
        else:
            tags = ''

        if self.source:
            source = ':%s ' % (self.source)
        else:
            source = ''

        verb = self.verb

        args = self.args_str()

        return ''.join((tags, source, verb, ' ', args))

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

