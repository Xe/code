-- Ask people who are using a guest nickname to pick one

pestered = {}

function suggestNick(line)
  local user = NewUser(line.Source)

  if user.Nick:match("pony_(.+)") then
    if pestered[user.Nick:lower()] == nil then
      bot.Notice(user.Nick, "Hi there " .. user.Nick .. "! You seem to be using a guest nickname. Please choose a nickname with /nick YourNickHere so it is easier to follow conversations.")
      pestered[user.Nick:lower()] = true
    end
  end
end

script.AddLuaProtohook(bot, "PRIVMSG", "suggestNick")
