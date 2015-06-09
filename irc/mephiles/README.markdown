Mephiles
========

[![GoDoc](https://godoc.org/github.com/Xe/mephiles?status.svg)](https://godoc.org/github.com/Xe/mephiles)

Mephiles is a new IRC daemon written in Go designed to replace ShadowIRCd
and/or Elemental-IRCd on networks that currently use it.

Abstract
--------

IRC is aging. People are slowly abandoning IRC to other programs or services
such as Skype and Google Hangouts. Mephiles will change this.

The eventual goal of Mephiles is to remain compatible with existing ShadowIRCd
and Elemental-IRCd networks while offering more tools for network owners to
easily manage, provision and scale their chat network with little effort.

Currently Mephiles is not functional as a RFC 1459 compliant irc daemon. Initial
efforts will be to get Mephiles to support the RFC 1459 client protocol and the
TS6 server linking protocol. Other improvements will be things like Websocket
support for client connections, transferring metadata with bans, and other
simple server protcol extensions that can't be done with the existing daemon.
A new server to server protocol will be made for Mephiles that is intentionally
incompatible with the exiting RFC 1459 framing.

Long-term Goals
---------------

All configuration for mephiles will be done via
[etcd](http://github.com/coreos/etcd). A command line tool for server operators
will be made to facilitate configuration changes. Additionally a tool for
converting existing ban databases and configurations from existing
Elemental-IRCd installations to the new format that heavily uses etcd keys.

Mephiles will be distributed as single binaries for all supported platforms as
well as an image on the Docker hub.
