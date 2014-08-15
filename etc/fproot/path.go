package main

import (
	"fmt"
	"strings"
)

func testPath(path string) (bool) {
	length := len(path)

	// Single slash test
	if length == 1 && path[0] == '/' {
		return true
	}

	// Local copy
	mypath := path

	// Strip trailing slash
	if path[(length-1)] == '/' {
		mypath = path[:(length-1)]
	}

	split := strings.Split(mypath, "/")[1:] // Uninterested in the first slash
	loc := 0

	for _, val := range split {
		if val == "." {
			continue
		} else if val == ".." {
			loc--
		} else {
			loc++
		}
	}

	if loc == 0 {
		return true
	}

	return false
}

func main () {
	singleSlash := testPath("/")
	folderTest := testPath("/bin/..")
	trickyTest := testPath("/./")
	expectedFalse := testPath("/bin/")

	fmt.Printf("/: %t, /bin/../ %t, /./: %t, /bin/: %t\n",
		singleSlash, folderTest, trickyTest, expectedFalse)
}

