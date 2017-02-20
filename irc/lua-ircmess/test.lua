require "ircmess"

for i=1, 50000 do
  s = ircmess.parse(":cyka.yolo-swag.com 001 Xena :Welcome to IRC!\r\n")

  assert(s.verb == "001")
  assert(s.source == "cyka.yolo-swag.com")
  assert(#s.argv == 2)
  assert(s.argv[1] == "Xena")

  s = ircmess.parse("PING :cyka.yolo-swag.com")

  assert(s.source == "")
  assert(s.verb == "PING")
  assert(#s.argv == 1)
  assert(s.argv[1] == "cyka.yolo-swag.com")

  s = ircmess.parse(":cyka.yolo-swag.com 003 Swaglogbot :This server was created Mon Apr 7 2014 at 2:06:11 EDT")

  assert(s.source == "cyka.yolo-swag.com")
  assert(s.verb == "003")
  assert(#s.argv == 2)
  assert(s.argv[1] == "Swaglogbot")
end

print("All tests passed!")

