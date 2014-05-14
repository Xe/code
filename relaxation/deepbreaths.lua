--Need the sleep function
require "socket"

function prompt(text)
  io.write(text.."> ")
  return io.stdin:read("*l")
end

function sleep(length)
  socket.select(nil, nil, length)
end

function speak(sentence)
  --os.execute("espeak \"" .. sentence .. "\" 2>&1 > /dev/null")
  print(sentence)
end

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

speak("Take a deep breath and get comfortable where you are. ")
sleep(3)

speak("Let's do a nice deep breathing excersize.  ")
sleep(2)

for i in range(10, 0, -1) do
  deepbreath()
end
