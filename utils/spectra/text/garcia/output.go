// Package garcia defines some simple output filters for text
// inspired by Agent Garcia's computer setup in Criminal Minds.
package garcia

import (
	"fmt"
	"strings"
)

// WriteHeader writes a heroku style header.
func WriteHeader(text interface{}) {
	fmt.Printf("> %s\n", text)
}

// WriteData spaces data out and writes it.
func WriteData(text interface{}) {
	fmt.Printf("\t> %s\n", text)
}

// WriteDataLevel allows for an arbitrary number of levels
// to layer text.
func WriteDataLevel(layers int, text interface{}) {
	space := strings.Repeat("\t", layers)
	fmt.Printf("%s> %s\n", space, text)
}

// WriteError signifies an error condition.
func WriteError(text interface{}) {
	fmt.Printf("[!ERROR!] %s [!ERROR!]\n", text)
}

// WriteEnd signifies the end of the build.
func WriteEnd(text interface{}) {
	fmt.Printf(">>>>>>> %s\n", text)
}
