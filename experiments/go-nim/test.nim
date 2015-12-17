import
  times,
  libcsum,
  libsum

let beginning = cpuTime()

echo "Starting Go FFI at " & $beginning

for i in countup(1, 100_000):
  let myi = i.cint
  discard libsum.add(myi, myi)

let endTime = cpuTime()

echo "Ended at " & $endTime
echo "Total: " & $(endTime - beginning)

let beforeGo = cpuTime()

echo "Doing the entire loop in Go. Starting at " & $beforeGo

libsum.addmanytimes()

let afterGo = cpuTime()

echo "Ended at " & $afterGo
echo "Total: " & $(afterGo - beforeGo) & " seconds"

echo "lol doing this in C now"

let cpre = cpuTime()
echo "starting C FFI at " & $cpre

for i in countup(1, 100_000):
  let myi = i.cint
  discard libcsum.add(myi, myi)

let cpost = cpuTime()

echo "Ended at " & $cpost
echo "Total: " & $(cpost - cpre)
