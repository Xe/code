FROM golang:latest

ADD . /go/src/github.com/Xe/betterbot/adaptors/telegram
RUN go get github.com/Xe/betterbot/adaptors/telegram/...

CMD telegram
