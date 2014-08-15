ZIP=dodrillshw1.zip

all:
	clear
	make clean
	gcc -o hw1 -g *.c

test:
	make
	./hw1 input.txt 1> output.txt 
	more output.txt

clean:
	touch hw1 4~
	rm hw1 *~

package:
	touch ${ZIP}
	rm ${ZIP}
	zip ${ZIP} *.c *.txt Makefile
