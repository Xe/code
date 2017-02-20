FROM golang:1.4.2

RUN go get github.com/constabulary/gb/...

ADD . /src
RUN cd /src &&\
    gb vendor update --all &&\
    gb build all &&\
    cp bin/* /go/bin

EXPOSE 6969

ONBUILD COPY cheese.cfg /cheese.cfg
CMD /go/bin/tomato-cheese -config /cheese.cfg
