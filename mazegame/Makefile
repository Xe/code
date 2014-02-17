ZIP=dodrillshw2.zip
BIN=mazegame

all:
	gcc -o ${BIN} -g maze.c game.c
	gcc -o music -g music.c

test:
	make clean
	make
	./${BIN} input.txt 1> output.txt 
	more output.txt

clean:
	touch ${BIN} 4~
	rm ${BIN} *~

package:
	make clean
	touch ${ZIP}
	rm ${ZIP}
	zip ${ZIP} *.c *.txt Makefile *.h
