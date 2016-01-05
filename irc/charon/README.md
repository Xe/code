charon
======

A simple ircd written in Go.

Requirements
------------

- Go
- gb for building (http://getgb.io/)
- Redis server

Building
--------

```console
$ git clone https://github.com/Xe/charon
$ gb build
```

Development
-----------

Source `./env.sh` for a quicker way to load the preferred GOPATH for things 
like vim.

Configuration
-------------

Copy `./doc/charon.default.cfg` to `charon.cfg` and edit it. You should edit 
the configuration before running Charon. If you don't and you break things it's 
not my fault.
