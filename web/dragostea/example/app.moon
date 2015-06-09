server = require "dragostea.server"
router = require "dragostea.router"

mux = router.Mux!

mux\get "/hello", (req) ->
  200, "hello #{req.method} from dragostea!"

with server.Server "9001"
  \serve mux
