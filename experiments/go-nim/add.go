package main

import "C"

//export add
func add(a, b int) int {
	return a + b
}

//export addmanytimes
func addmanytimes() {
	for i := 0; i < 100000; i++ {
		add(i, i)
	}
}

func main() {}
