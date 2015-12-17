Calling Go from Nim (technically C)
===================================

The FFI barrier seems really massive. It's best to delegate as much work to Go 
as possible.

```
xena@fluttershy (linux) ~/code/code/experiments/go-nim
➜  ./build.sh
+ go build -buildmode=c-shared -o libsum.so add.go
+ gcc -c -Wall -Werror -fPIC ./libcsum.c
+ gcc -shared -o libcsum.so libcsum.o
+ nim c -d:release -r test.nim
Hint: system [Processing]
Hint: test [Processing]
Hint: times [Processing]
Hint: strutils [Processing]
Hint: parseutils [Processing]
Hint: libcsum [Processing]
Hint: libsum [Processing]
Hint:  [Link]
Hint: operation successful (13455 lines compiled; 0.201 sec total; 14.143MB; Release Build) [SuccessX]
Starting Go FFI at 0.001177
Ended at 0.164998
Total: 0.163821
Doing the entire loop in Go. Starting at 0.165024
Ended at 0.165109
Total: 8.500000000000174e-05 seconds
lol doing this in C now
starting C FFI at 0.16512
Ended at 0.165385
Total: 0.0002650000000000152
```
