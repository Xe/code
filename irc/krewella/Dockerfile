FROM golang:1.4.1

ENV GOPATH /go
ADD . /go/src/christine.website/go/krewella
RUN go get christine.website/go/krewella/...

EXPOSE 80
ENV PORT 80

CMD krewella
