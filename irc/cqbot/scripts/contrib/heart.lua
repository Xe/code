function heart(line)
  if line.Args[2]:find("❤") then
    bot.Say(line.Args[1], "❤")
  end
end

script.AddLuaProtohook(bot, "PRIVMSG", "heart")

