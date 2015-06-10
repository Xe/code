all: build

build:
	go build main.go
	gcc -lwiringPi -o dht11 dht11.c

run: build
	sudo ./main
