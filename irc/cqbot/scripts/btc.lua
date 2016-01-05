local json = require "json"
local http = require "socket.http"

function btc(user, message)
  local request = http.request("https://www.bitstamp.net/api/ticker/")
  local info = json.decode(request)
  return "Bitstamp prices: "..info.ask.." average, "..info.low.." low, "..info.high.." high"
end

script.AddLuaCommand(bot, "BTC", "Bitcoin price lookups", "btc")
