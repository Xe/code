pegasus = require "pegasus"

server = pegasus\new os.getenv "PORT" or "9001"

class Server
  new: (port) =>
    @server = pegasus\new port or os.getenv "PORT" or "9001"

  serve: (router) =>
    @server\start (req, reply) ->
      assert router\execute req, reply

{
  :Server
}
