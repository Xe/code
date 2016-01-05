core = require "charon/core"

describe "core structures: ", ->
  describe "channel", ->
    it "should be able to be created", ->
      with core.Channel "#myChannel", os.time!
        _ = nil

    it "should have a proper constructor", ->
      with core.Channel "#myChannel", os.time!
        assert .name ~= .sortName
        assert .epoch ~= 0

    it "should be able to serialize from redis", ->
      with core.Channel.from_redis {
          topic: "test channel"
          topictime: 42069
          topichost: "foo!bar@baz"
          modes: "+nt"
          name: "#myChannel"
          epoch: 1434877333
      }
        assert .topic == "test channel"
        assert .topicEpoch == 42069
        assert .topicHost == "foo!bar@baz"
        assert .modes == "+nt"
        assert .name == "#myChannel"
        assert .sortName == "#mychannel"
        assert .epoch == 1434877333
