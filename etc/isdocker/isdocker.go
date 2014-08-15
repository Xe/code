package isdocker

import (
	"bufio"
	"os"
)

// http://stackoverflow.com/a/12206584
// Readln returns a single line (without the ending \n)
// from the input buffered reader.
// An error is returned iff there is an error with the
// buffered reader.
func Readln(r *bufio.Reader) (string, error) {
	var (
		isPrefix bool  = true
		err      error = nil
		line, ln []byte
	)
	for isPrefix && err == nil {
		line, isPrefix, err = r.ReadLine()
		ln = append(ln, line...)
	}
	return string(ln), err
}

// Returns true if you are in a docker container and false if you are not.
func Contained() bool {
	fin, _ := os.Open("/proc/1/cgroup")
	reader := bufio.NewReader(fin)
	Readln(reader)
	line, _ := Readln(reader)

	if len(line) > 16 { //HACK
		return true
	}

	return false
}
