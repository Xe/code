FROM flitter/init

RUN apt-get update && apt-get -y install git

RUN cd /usr/local && wget https://storage.googleapis.com/golang/go1.4.2.linux-amd64.tar.gz && \
    tar xf go1.4.2.linux-amd64.tar.gz && rm go1.4.2.linux-amd64.tar.gz

RUN mkdir /go
ENV GOPATH /go
ENV PATH $PATH:/usr/local/go/bin:/go/bin

ADD . /go/src/github.com/Xe/yolon
RUN cd /go/src/github.com/Xe/yolon && go get ./...

ADD run/run /etc/service/yolon/run

CMD /sbin/my_init

EXPOSE 3000
