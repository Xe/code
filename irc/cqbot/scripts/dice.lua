function rollDice(user, message)
  if #message < 1 then
    return "Need a kind of dice to roll! (try 1d20)"
  end

  if message[1]:upper() == "RICK" then
    return "https://www.youtube.com/watch?v=dQw4w9WgXcQ"
  end

  if table.concat(luar.slice2table(message), " "):match("they see me") then
    return "https://www.youtube.com/watch?v=N9qYF9DZPdw"
  end

  number, kind = message[1]:match("(%d+)d(%d+)")

  number = tonumber(number)
  if number == nil then
    number = 1
  end

  kind = tonumber(kind)
  if kind == nil then
    return "Invalid kind of die! Try a number."
  end

  result = 0

  for i = 1,number do
    result = result + math.random(kind)
  end

  return "Dice roll of " .. number .. " " .. kind .. "-sided dice: " .. result
end

math.randomseed(os.time())

register_command("ROLL", "ROLL [number]<dice kind>: simulates a dice roll", "rollDice")

