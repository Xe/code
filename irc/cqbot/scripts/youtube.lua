-- Based on:
-- https://github.com/TheLinx/Juiz/blob/master/modules/ytvlookup.lua

local json = require "json"
local http = require "socket.http"

function ytlookup(id)
  local c = http.request("http://gdata.youtube.com/feeds/api/videos/"..id.."?alt=json&fields=author,title")
  local res = json.decode(c)
  local author, title = res.entry.author[1].name["$t"], res.entry.title["$t"]

  return "^ Youtube - " .. title .. " Posted by: " .. author
end

function youtube_scrape(line)
  local message = line.Args[2]

  if message:find("youtube%.com/watch") then
    bot.Say(line.Args[1], ytlookup(message:match("v=(...........)")))
  elseif message:find("youtu%.be/") then
    bot.Say(line.Args[1], ytlookup(message:match("%.be/(...........)")))
  else return end
end

function youtube_search(user, message)
  local search = table.concat(luar.slice2table(message), " ")
  search = url_encode(search)
  local url = "https://gdata.youtube.com/feeds/api/videos?q="..search.."&v=2&alt=jsonc"
  local c = http.request(url)
  local json = json.decode(c)
  local video = json.data.items[1]
  return "Youtube: " .. video.title .. " uploaded by " .. video.uploader .. " http://youtu.be/" .. video.id
end

script.AddLuaProtohook(bot, "PRIVMSG", "youtube_scrape")
register_command("YT", "Searches youtube for content", "youtube_search")

