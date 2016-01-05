require "ircmess"
redis = require "redis"

params =
  host: os.getenv("REDIS_HOST") or "127.0.0.1"
  port: os.getenv("REDIS_PORT") or 6379

client = redis.connect params
client\select 15

users = {}

print "Connected to redis"

channels = { "global" }

for msg, abort in client\pubsub subscribe: channels

  if msg.kind == "subscribe"
    print "Subscribed to #{msg.channel}"

  elseif msg.kind == "message"
    print msg.payload

    line = ircmess.parse msg.payload

    if line.argv[3] == "!quit"
      abort!

    if line.verb == "PRIVMSG"
      print "#{line.argv[1]} told #{line.argv[2]}: \"#{line.argv[3]}\""
