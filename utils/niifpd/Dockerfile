FROM alpine

RUN apk update && apk add python

ADD . /niifpd

WORKDIR /niifpd
CMD python niifpd.py
EXPOSE 8430
