FROM golang:1.4.2

RUN go get github.com/constabulary/gb/...

ADD . /src
RUN cd /src && gb build all && cp bin/* /go/bin

ONBUILD COPY charon.cfg /charon.cfg
CMD charon -cfg /charon.cfg
