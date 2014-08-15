ZIP=dodrillshw2.zip

all:
	javac *.java

test:
	make
	java QueueTester input.txt 30 40 > output.txt
	less output.txt

package:
	make clean
	zip ${ZIP} *.java *.txt Makefile

clean:
	rm *.class
