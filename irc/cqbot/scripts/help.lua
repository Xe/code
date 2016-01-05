VERB="HELP"
HELP="HELP <COMMAND> shows help for a command"

function command_list()
  local ret = ""

  for key, command in pairs(bot.Commands) do
    if ret ~= "" then
      ret = ret .. ", " .. command.Verb
    else
      ret = command.Verb
    end
  end

  return ret
end

function show_help(user, message)
  if #message > 0 then
    -- If user provided params
    local verb = string.upper(message[1])

    if bot.Commands[verb] ~= nil then
      return bot.Commands[verb].Help
    else
      return "No such command " .. verb
    end

  else
    -- Otherwise show a list of commands
    bot.Notice(user.Nick, command_list())
    return "Need a command name, " .. HELP
  end
end

script.AddLuaCommand(bot, VERB, HELP, "show_help")
