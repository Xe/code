FROM golang:latest

ADD . /go/src/github.com/Xe/betterbot/adaptors/matrix
RUN go get github.com/Xe/betterbot/adaptors/matrix/...

CMD matrix
