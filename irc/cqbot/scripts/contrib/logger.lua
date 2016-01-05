funcs = {
  PRIVMSG = function(line)
    local client = NewUser(line.Source)
    return "<" .. client.Nick .. "> " .. line.Args[2]
  end,
  JOIN = function(line)
    local client = NewUser(line.Source)
    return sprintf("--> %s [%s@%s] joined %s", client.Nick, client.User,
      client.Host, line.Args[1])
  end,
  PART = function(line)
    local client = NewUser(line.Source)
    return sprintf("<-- %s left %s (%s)", client.Nick, line.Args[1], line.Args[2])
  end,
  KICK = function(line)
    local client = NewUser(line.Source)
    return sprintf("<-- %s was kicked from %s by %s (%s)", line.Args[2],
      line.Args[1], client.Nick, line.Args[3])
  end,
  QUIT = function(line)
    local client = NewUser(line.Source)
    return sprintf("%s [%s@%s] quit (%s)", client.Nick, client.User, client.Host, line.Args[1])
  end,
  MODE = function(line)
    local source = line.Source
    if source:match("@") then
      local client = NewUser(source)
      return sprintf("%s set mode %s", client.Nick, table.concat(luar.slice2table(line.Args), " "))
    else
      return sprintf("%s set mode %s", source, table.concat(luar.slice2table(line.Args), " "))
    end
  end,
}

function logger(line)
  local ret = funcs[line.Verb](line)
  log(ret)
end

function log(data)
  printf(data)

  local fout = io.open("var/cqbot." .. bot.Config.Server.Host .. ".log", "a")
  fout:write(os.date("%x %H:%M:%S ") .. data .. "\n")
  fout:close()
end

script.AddLuaProtohook(bot, "PRIVMSG", "logger")
script.AddLuaProtohook(bot, "JOIN", "logger")
script.AddLuaProtohook(bot, "PART", "logger")
script.AddLuaProtohook(bot, "KICK", "logger")
script.AddLuaProtohook(bot, "QUIT", "logger")
script.AddLuaProtohook(bot, "MODE", "logger")

log("Log opened")
log("Logging: " .. table.concat(keys(funcs), " "))
