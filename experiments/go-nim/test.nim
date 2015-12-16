import
  times,
  libsum

let beginning = cpuTime()

echo "Starting Go FFI at " & $beginning

for i in countup(1, 100_000):
  discard libsum.add(i.cint, i.cint)

let endTime = cpuTime()

echo "Ended at " & $endTime

echo "Total: " & $(endTime - beginning)
