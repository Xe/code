package main

import (
	"bytes"
	"fmt"
	"io"
	"strings"

	"github.com/dchest/blake2b"
)

func CloakIP6(text string) string {
	stuff := strings.Split(text, ":")
	stuff[len(stuff)-1] = CloakString(stuff[len(stuff)-1], config.Server.Salt)
	if stuff[len(stuff)-2] != "" {
		stuff[len(stuff)-2] = CloakString(stuff[len(stuff)-2], config.Server.Salt)
	} else {
		stuff[len(stuff)-3] = CloakString(stuff[len(stuff)-3], config.Server.Salt)
	}
	text = strings.Join(stuff, ":")
	return text
}

func CloakIP4(text string) string {
	stuff := strings.Split(text, ".")
	stuff[len(stuff)-1] = CloakString(stuff[len(stuff)-1], config.Server.Salt)
	stuff[len(stuff)-2] = CloakString(stuff[len(stuff)-2], config.Server.Salt+stuff[len(stuff)-2])
	text = strings.Join(stuff, ".")
	return text
}

func CloakHost(text string) string {
	stuff := strings.Split(text, ".")
	stuff[0] = CloakString(stuff[0], config.Server.Salt+text)
	text = strings.Join(stuff, ".")
	return text
}

func CloakString(text string, salt string) string {
	r := string(Blake2bString(text + salt))
	for len(r) < len(text) {
		r = r + Blake2bString(r)
	}

	side := true
	for len(r) > len(text) {
		if side {
			r = r[1:]
			side = false
		} else {
			r = r[:len(r)-1]
			side = true
		}
	}

	return r
}

func Blake2bString(text string) string {
	c := &blake2b.Config{
		Salt: []byte(config.Server.Salt),
		Size: blake2b.Size,
	}

	b2b, err := blake2b.New(c)
	if err != nil {
		panic(err)
	}

	b2b.Reset()

	fin := bytes.NewBufferString(text)
	io.Copy(b2b, fin)

	result := fmt.Sprintf("%x", b2b.Sum(nil))
	return result
}
