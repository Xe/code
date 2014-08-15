ZIP=dodrillshw2.zip
BIN=hw2

all:
	clear
	make clean
	gcc -o ${BIN} -g maze.c cscd*.c
	gcc -o music -g music.c

test:
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
