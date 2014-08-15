require "irc"
local sleep = require "socket".sleep

local s = irc.new{nick = "example"}

s:hook("OnChat", function(user, channel, message)
  print(("[%s] %s: %s"):format(channel, user.nick, message))
end)

s:hook( "OnChat", function(user, channel, message)
  if string.find(message, "Ashley", 1) then
    print "This should cause a notification"
  end
end)

s:connect("irc.yolo-swag.com")
s:join("#niichan")

while true do
  s:think()
  sleep(0.1)
end
