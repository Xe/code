// +build debug

package server

import "log"

func debug(args ...interface{}) {
	log.Print(args...)
}

func debugf(format string, args ...interface{}) {
	log.Printf(format, args...)
}
