local json = require "json"
local http = require "socket.http"

function dblookup(id)
  local url = "http://derpiboo.ru/"..id..".json?nocomments"
  printf("url: %s", url)
  local c = http.request {
    url = url,
    headers = {
      ["user-agent"] = "CQBot (Go+Lua!)"
    },
  }
  printf("json: %#v", c)
  local obj = json.decode(c)

  return obj
end

function summarize(info)
  local ret = "^ Derpibooru: "
  if info.tags.explicit then ret = ret .. "[NSFW] " else ret = ret .. "[SFW] " end
  ret = ret .. "Tags: " .. table.concat(info.tags, " ")

  return ret
end

function db_scrape(line)
  local message = line.Args[2]

  if message:find("derpiboo.ru") then
    local id = message:match("/(%d+)")
    printf("id: %s", id)
    local info = dblookup(id)

    if info.tags.explicit ~= nil then
      bot.Notice("@"..line.Args[1], string.format("%s posted a NSFW link!", user.Nick))
    end

    bot.Say(line.Args[1], summarize(id))
  end
end

script.AddLuaProtohook(bot, "PRIVMSG", "db_scrape")
