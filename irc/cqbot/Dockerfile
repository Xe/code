FROM flitter/init

RUN apt-get update && apt-get install -qy --no-install-recommends build-essential gcc golang liblua5.1-0-dev liblua50-dev liblualib50-dev git mercurial luarocks unzip
RUN luarocks install moonscript &&\
    luarocks install luajson &&\
    luarocks install luasocket

ENV DOCKER YES
ENV CQBOT_CONF /cqbot.conf
ENV GOPATH /go
ENV PATH $PATH:/go/bin

# Add the source code and lua scripts
ADD . /go/src/github.com/Xe/cqbot
ADD start.sh /etc/service/cqbot/run
ADD cqbot.conf /

RUN cd /go/src/github.com/Xe/cqbot && go get -v ./...

CMD /sbin/my_init
