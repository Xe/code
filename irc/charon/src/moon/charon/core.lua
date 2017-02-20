local Channel
do
  local _class_0
  local _base_0 = {
    name = "",
    sortName = "",
    epoch = 0,
    modes = "",
    metadata = { },
    topic = "",
    topicHost = "",
    topicEpoch = 0,
    members = { },
    from_redis = function(arg)
      do
        local _with_0 = Channel(arg.name, arg.epoch)
        _with_0.topic = arg.topic
        _with_0.topicEpoch = arg.topictime
        _with_0.topicHost = arg.topichost
        _with_0.modes = arg.modes
        return _with_0
      end
    end,
    to_redis = function(self)
      return {
        "charon:channel:" .. tostring(self.sortName),
        "topic",
        self.topic,
        "topichost",
        self.topicHost,
        "topictime",
        self.topicEpoch,
        "modes",
        self.modes,
        "name",
        self.name
      }
    end
  }
  _base_0.__index = _base_0
  _class_0 = setmetatable({
    __init = function(self, name, epoch)
      if epoch == nil then
        epoch = os.time()
      end
      self.name = name
      self.sortName = name:lower()
      self.epoch = epoch
    end,
    __base = _base_0,
    __name = "Channel"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  Channel = _class_0
end
local Server
do
  local _class_0
  local _base_0 = {
    name = "",
    uuid = "",
    description = "",
    kind = "",
    epoch = 0
  }
  _base_0.__index = _base_0
  _class_0 = setmetatable({
    __init = function() end,
    __base = _base_0,
    __name = "Server"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  Server = _class_0
end
local User
do
  local _class_0
  local _base_0 = {
    nick = "",
    ident = "",
    host = "",
    vhost = "",
    gecos = "",
    realhost = "",
    ip = "",
    uuid = "",
    sid = "",
    oper = false,
    service = false
  }
  _base_0.__index = _base_0
  _class_0 = setmetatable({
    __init = function() end,
    __base = _base_0,
    __name = "User"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  User = _class_0
end
return {
  Channel = Channel,
  Server = Server,
  User = User
}
