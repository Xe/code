#!/bin/sh

export TCLLIBPATH='/usr/lib/tcl8.6 /usr/lib'

wapptclsh mkdb.tcl
wapptclsh site.tcl --server $PORT
