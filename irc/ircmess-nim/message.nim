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

proc `$`*(m: Message): string =
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
  # TODO: write a testing module that pukes out tap
  echo "1..5"

  var
    m1: Message
    m2: Message
    m3: Message

  try:
    m1 = parseMessage ":hi foo bar baz :this is a longer message :with another colon"
    m2 = parseMessage ":hi foo bar baz this has no swag"
    m3 = parseMessage "PING :sonatadusk.ponychat.net"
    echo "ok 1 - all messages could be parsed"
    echo "  m1: ", m1
    echo "  m2: ", m2
    echo "  m3: ", m3
  except:
    echo "not ok 1 - all messages could be parsed"
    echo "  " & getCurrentExceptionMsg()

  try:
    assert(m1.source.cmp("hi") == 0)
    assert(m2.source.cmp("hi") == 0)
    echo "ok 2 - compare source of messages"
  except:
    echo "not ok 2 - compare source of messages"
    echo "  " & $ m1.source
    echo "  " & $ m2.source

  try:
    assert(m1.verb.cmp("FOO") == 0)
    assert(m2.verb.cmp("FOO") == 0)
    echo "ok 3 - compare verb of messages"
  except:
    echo "not ok 3 - compare verb of messages"
    echo "  " & $ m1.verb
    echo "  " & $ m2.verb

  try:
    assert(m1.args[0].cmp("bar") == 0)
    assert(m1.args[1].cmp("baz") == 0)
    assert(m1.args[2].cmp("this is a longer message :with another colon") == 0)
    echo "ok 4 - message with random colon in it"
  except:
    echo "not ok 4 - message with random colon in it"
    echo "  " & $ m1.args

  try:
    assert(m2.args[0].cmp("bar") == 0)
    assert(m2.args[1].cmp("baz") == 0)
    assert(m2.args[2].cmp("this") == 0)
    assert(m2.args[3].cmp("has") == 0)
    assert(m2.args[4].cmp("no") == 0)
    assert(m2.args[5].cmp("swag") == 0)
    echo "ok 5 - message with many arguments"
  except:
    echo "not ok 4 - message with many arguments"
    echo "  " & $ m2.args
