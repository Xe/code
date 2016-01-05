build:
	go build .
	strip cqbot
	docker build -t cqbot .

deps:
	go get github.com/stevedonovan/luar
	go get github.com/aarzilli/golua/lua
	go get code.google.com/p/gcfg
	go get code.google.com/p/go-uuid/uuid
	sudo luarocks install moonscript
	sudo luarocks install luajson
	sudo luarocks install luasocket

run:
	@make build
	docker run --rm -it --name cqbot cqbot

run-test:
	cd bot; go test

