-- Fix nickname when trying to connect and repair it when clones die off

function renick(line)
  local nick = bot.GetNick() .. "_"
  bot.SetNick(nick)
end

function fixnick(line)
  local nick = bot.GetNick()
  local client = NewUser(line.Source)
  if client.Nick == bot.Config.Bot.Nick and nick ~= bot.Config.Bot.Nick
  then
    bot.SetNick(bot.Config.Bot.Nick)
  end
end

script.AddLuaProtohook(bot, "433", "renick")
script.AddLuaProtohook(bot, "NICK", "fixnick")
script.AddLuaProtohook(bot, "QUIT", "fixnick")
