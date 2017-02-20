FROM golang:1.4

RUN go get golang.org/x/tools/cmd/present

COPY ./docker.slide /xe/docker.slide

EXPOSE 80

WORKDIR /xe
CMD present -http 0.0.0.0:80
