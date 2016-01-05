class Channel
  name: "" -- string
  sortName: "" -- string

  epoch: 0 -- number
  modes: "" -- string
  metadata: {} -- table[string] -> string

  topic: "" -- string
  topicHost: "" -- string
  topicEpoch: 0 -- number

  members: {} -- table[string] -> User

  new: (name, epoch=os.time!) =>
    @name = name
    @sortName = name\lower!
    @epoch = epoch

  from_redis: (arg) ->
    with Channel arg.name, arg.epoch
      .topic = arg.topic
      .topicEpoch = arg.topictime
      .topicHost = arg.topichost
      .modes = arg.modes

  to_redis: =>
    {
      "charon:channel:#{@sortName}",
      "topic", @topic,
      "topichost", @topicHost,
      "topictime", @topicEpoch,
      "modes", @modes,
      "name", @name,
    }

class Server
  name: "" -- string
  uuid: "" -- string
  description: "" -- string
  kind: "" -- kind of server
  epoch: 0 -- number

class User
  nick: "" -- string
  ident: "" -- string
  host: "" -- string
  vhost: "" -- string
  gecos: "" -- string
  realhost: "" -- string
  ip: "" -- string

  uuid: "" -- string
  sid: "" -- string

  oper: false -- boolean
  service: false -- boolean

{ :Channel, :Server, :User }
