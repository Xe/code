pattern = "f[%@a]gi?g?[oeu]t?[zs]?"

count = 0

-- foo!bar@baz PRIVMSG #bez :yes hello
function kickOnFag(line)
  local user = NewUser(line.Source)
  local channel = line.Args[1]
  local message = line.Args[2]

  if message:match(pattern) then
    count = count +1
    bot.SendLine("KICK " .. channel .. " " .. user.Nick .. ":Please do not use that word here. Thank you. [#" .. count .. "]")
  end
end

script.AddLuaProtohook(bot, "PRIVMSG", "kickOnFag")
