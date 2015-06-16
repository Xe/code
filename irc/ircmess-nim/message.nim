import strutils

type
  Message* = object
    source*: string
    verb*: string
    args*: seq[string]

proc parseMessage*(input: string): Message =
  ## Takes in a string and returns a Message from it

  var
    splitline = input.split(' ')
    source = ""
    verb = ""
    args: seq[string]

  # parse out the source
  if splitline[0][0] == ':':
    source = splitline[0][1 .. ^1]
    verb = splitline[1].toUpper()
    splitline = splitline[2 .. ^1]
  else:
    # sourceless message
    source = ""
    verb = splitline[0]

    splitline = splitline[1 .. ^1]

    if splitline[0][0] == ':':
      # Only one argument, figure this out and return it
      args = @[splitline[0][1 .. ^1]]

      return Message(source: source, verb: verb, args: args)

  # Arguments are next
  var argstring = splitline.join(" ")
  var extparam = argstring.split(" :")

  # figure out where the extparam is and append it to the end of the argument list raw
  if extparam.len() > 1:
    var
      dupes = extparam[1 .. ^1]
      ext = dupes.join(" :")
      myArgs = extparam[0].split(" ")

    args = myArgs & ext
  else:
    # no extparam
    args = splitline

  Message(source: source, verb: verb, args: args)

proc `$$`*(m: Message): string =
  ## Show converts a Message to a string format, dictated by RFC 1459
  var r = ""

  # Prepend message source if applicable
  # :nick!user@host
  if m.source != "":
    r = ":" & m.source & " "

  # Add the message's verb
  r = r & m.verb & " "

  # append everything but the last argument to this
  for i in countup(0, len(m.args)-2):
    r = r & m.args[i] & " "

  # last paramater is the extended parameter
  r & ":" & m.args[(len(m.args)-1)]

when isMainModule:
  import unittest

  suite "ircmess tests":
    var
      m1: Message
      m2: Message
      m3: Message

    test "basic message parsing":
      try:
        m1 = parseMessage ":hi foo bar baz :this is a longer message :with another colon"
        m2 = parseMessage ":hi foo bar baz this has no swag"
        m3 = parseMessage "PING :sonatadusk.ponychat.net"
      except:
        fail
        echo getCurrentExceptionMsg()

    test "source parsing":
      try:
        check(m1.source == "hi")
        check(m2.source == "hi")
      except:
        fail
        echo "  " & $ m1.source
        echo "  " & $ m2.source

    test "verb parsing":
      try:
        check(m1.verb.cmp("FOO") == 0)
        check(m2.verb.cmp("FOO") == 0)
      except:
        fail
        echo "  " & $ m1.verb
        echo "  " & $ m2.verb

    test "arg parsing with exceptional case":
      try:
        check(m1.args[0].cmp("bar") == 0)
        check(m1.args[1].cmp("baz") == 0)
        check(m1.args[2].cmp("this is a longer message :with another colon") == 0)
      except:
        fail
        echo "  " & $ m1.args

    test "general arg parsing":
      try:
        assert(m2.args[0].cmp("bar") == 0)
        assert(m2.args[1].cmp("baz") == 0)
        assert(m2.args[2].cmp("this") == 0)
        assert(m2.args[3].cmp("has") == 0)
        assert(m2.args[4].cmp("no") == 0)
        assert(m2.args[5].cmp("swag") == 0)
      except:
        fail
        echo "  " & $ m2.args
