--Need the sleep function
require "socket"

--- Prompt user input
-- Returns a line from the user
function prompt(text)
  io.write(text.."> ")
  return io.stdin:read("*l")
end

--- Sleep for length time
-- Uses socket.select to sleep for a length of time
function sleep(length)
  socket.select(nil, nil, length)
end

--- Say a line
-- In the future this will also speak lines out loud using a text to
-- speech engine
function speak(sentence)
  --os.execute("espeak \"" .. sentence .. "\" 2>&1 > /dev/null")
  print(sentence)
end

--- Range iterator
-- Like python's range
function range(from, to, step)
  step = step or 1
  return function(_, lastvalue)
    local nextvalue = lastvalue + step
    if step > 0 and nextvalue <= to or step < 0 and nextvalue >= to or
      step == 0
      then
        return nextvalue
      end
    end, nil, from - step
  end

--- Timing for deep breaths
-- an 18 second breathing cycle
function deepbreath()
  speak("Deep breath in")
  sleep(2)

  speak("still going in... ")
  sleep(2)

  speak("and in... ")
  sleep(2)

  speak("just a bit more...")
  sleep(2)

  speak("Now hold it")
  sleep(2)

  speak("and exhale slowly... ")
  sleep(2)

  speak("keep going... ")
  sleep(2)

  speak("almost done... ")
  sleep(2)

  speak("and out.")
  sleep(2)

  print()
end

-- Main body of script

speak("Take a deep breath and get comfortable where you are. ")
sleep(3)

speak("Let's do a nice deep breathing excersize.")
sleep(2)

-- 10 deep breaths is enough for me
for i in range(10, 0, -1) do
  deepbreath()
end
