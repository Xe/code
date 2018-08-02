FROM xena/alpine

RUN apk --no-cache add wapptclsh tcl-tls

COPY . /app

EXPOSE 8080
ENV PORT 8080
VOLUME /data
ENV DATABASE_PATH /data/measurements.db
WORKDIR /app
CMD ./run.sh
