dev:
	docker build -t xena/unimatrix-dev .
	docker run --rm --name="unimatrix-dev" -v `pwd`/data:/src/data -p 5000:5000 xena/unimatrix-dev
	make kill

db:
	docker run --name unimatrix-flatfile -v /src/data -d busybox /bin/true

push:
	docker push xena/unimatrix-dev

kill:
	docker stop unimatrix-dev
	docker rm unimatrix-dev
