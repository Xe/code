#!/bin/bash

cd /go/src/github.com/Xe/cqbot

go get ./...
go build .

/go/bin/cqbot
