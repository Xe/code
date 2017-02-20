.PHONY: all

all:
	docker build -t xena/dokku-brownbag .
	docker run --rm -itp 80:80 xena/dokku-brownbag
