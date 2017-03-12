FROM golang:latest

ADD . /go/src/github.com/Xe/betterbot/adaptors/irc
RUN go get github.com/Xe/betterbot/adaptors/irc/...

CMD irc
