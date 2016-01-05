moonscript = require "moonscript.base"

eval_moon =
 elevated() ..
 function(user, message)
  message = luar.slice2table(message)
  message = table.concat(message, " ")
  --printf("%#v", moonscript.to_lua(message))
  code = moonscript.loadstring(message)
  if code == nil then
    printf("%#v", moonscript.to_lua(message))
    return "Moonscript error!"
  else
    code()
  end

  return "Code ran without error"
end

register_command("MOON", "Evaluate moonscript!", "eval_moon")

