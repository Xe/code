AS=das
CC=clang-dcpu
EMU=dcpu
EMUFLAGS=-l
PROGRAM=mazegame

all:
	$(AS) $(PROGRAM).dasm -o $(PROGRAM).hex

run:
	$(EMU) $(EMUFLAGS) $(PROGRAM).hex

debug:
	make run EMUFLAGS=-d

.PHONY: all run debug
