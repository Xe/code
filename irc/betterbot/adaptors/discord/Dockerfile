FROM golang:latest

ADD . /go/src/github.com/Xe/betterbot/adaptors/discord
RUN go get github.com/Xe/betterbot/adaptors/discord/...

CMD discord
