FROM golang:alpine

RUN apk add --update git \
&& go get -v -u github.com/nats-io/nats-top \
&& apk del --purge git

CMD /go/bin/nats-top -s nats
