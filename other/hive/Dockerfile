FROM golang:alpine

# Add Tini
ENV TINI_VERSION v0.9.0
ADD https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini-static /tini
RUN chmod +x /tini
ENTRYPOINT ["/tini", "--"]

ADD ./glide.yaml /go/src/github.com/Xe/hive/glide.yaml
ADD ./glide.lock /go/src/github.com/Xe/hive/glide.lock

RUN apk add --update git \
&& go get -u -v github.com/Masterminds/glide \
&& cd /go/src/github.com/Xe/hive && glide install \
&& apk del --purge git mercurial

ADD . /go/src/github.com/Xe/hive
RUN go get github.com/Xe/hive/cmd/... \
&& cp /go/src/github.com/Xe/hive/tools/makechilds.sh /usr/bin/makechilds
