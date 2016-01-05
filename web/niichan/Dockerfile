FROM coopernurse/docker-nim

RUN apk update && apk add libpq

ADD . /app

WORKDIR /app
EXPOSE 8080

CMD niichan

RUN nim c -o:../bin/provisiondb ./tools/provisiondb
RUN yes | nimble install
