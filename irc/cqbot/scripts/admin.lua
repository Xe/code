load_script =
 elevated() ..
 function (user, message)
  local sname = message[1]

  if #message > 0 then
    bot.LoadScript(sname)
    bot.Log.Printf("%s loaded script %s", user.Nick, sname)
    return "Loaded "..message[1]
  else
    return "Need script name"
  end
end

unload_script =
 elevated() ..
 function (user, message)
  local sname = message[1]..".lua"

  if sname == "admin.lua" then
    return "Can't do that"
  end

  if #message > 0 then
    bot.UnloadScript(sname)
    bot.Log.Printf("%s unloaded script %s", user.Nick, sname)
    return "Unloaded "..sname
  else
    return "Need script name"
  end
end

reload_script =
 elevated() ..
 function (user, message)
  local sname = message[1]..".lua"

  if #message < 0 then
    return "Need script name to reload"
  end

  bot.UnloadScript(sname)
  bot.LoadScript(sname)

  return sname .. " reloaded"
end


list_scripts =
 elevated() ..
 function(user, message)
  for key, script in pairs(bot.Scripts) do
    printf("%s: %#v", key, script)
  end

  return "List logged"
end

die =
 elevated() ..
 function (user, message)
  bot.Log.Printf("Asked to die")
  bot.Quit("Xena killed me :(")
  return "Done."
end

function version(user, line)
  local commit = os.capture("git rev-parse --short HEAD")
  return "CQBot version 0.1-"..commit
end

script.AddLuaCommand(bot, "LOAD", "LOAD <script> loads a script into memory", "load_script")
script.AddLuaCommand(bot, "UNLOAD", "UNLOAD <script> unloads a script from memory", "unload_script")
script.AddLuaCommand(bot, "RELOAD", "RELOAD <script> reloads a script", "reload_script")
script.AddLuaCommand(bot, "DIE", "Kills the bot", "die")
script.AddLuaCommand(bot, "LIST", "Shows scripts that are loaded", "list_scripts")
script.AddLuaCommand(bot, "VERSION", "Shows version information", "version")
