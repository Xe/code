package main

import (
	"fmt"
	"strings"
)

// https://gist.github.com/bemasher/1777766

type Stack struct {
	top *Element
	size int
}

type Element struct {
	value interface{}
	next *Element
}

// Return the stack's length
func (s *Stack) Len() int {
	return s.size
}

// Push a new element onto the stack
func (s *Stack) Push(value interface{}) {
	s.top = &Element{value, s.top}
	s.size++
}

// Remove the top element from the stack and return it's value
// If the stack is empty, return nil
func (s *Stack) Pop() (value interface{}) {
	if s.size > 0 {
		value, s.top = s.top.value, s.top.next
		s.size--
		return
	}
	return nil
}

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
	loc := new(Stack)

	for _, val := range split {
		if val == "." {
			continue
		} else if val == ".." {
			loc.Pop()
		} else {
			loc.Push(val)
		}
	}

	if loc.Len() == 0 {
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

