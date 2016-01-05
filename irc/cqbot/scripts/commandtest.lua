--- Test command
--  @param user The User object that sourced the line
--  @param message A table of the parameters (space separated) the user provided
--  @return The message string to send to the user
function test(user, message)
  return "Hi from lua!"
end

elevatedtest =
 elevated() ..
 function(user, message)
  return "Hi master"
end

register_command("TEST", "Test lua command!", "test")
register_command("ELVTEST", "Test elevated command", "elevatedtest")

