package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"log"
	"net/textproto"
	"os/exec"
)

type Content struct {
	Humidity float32 `json:"humidity"`
	C        float32 `json:"c"`
	F        float32 `json:"f"`
}

func main() {
	cmd := exec.Command("./dht11")

	bb := &bytes.Buffer{}
	cmd.Stdout = bb

	err := cmd.Start()
	if err != nil {
		log.Fatalf("command start %#v", err)
	}

	r := bufio.NewReader(bb)
	tp := textproto.NewReader(r)

	for {
		line, err := tp.ReadLine()
		if err != nil {
			log.Fatalf("readline %#v", err)
		}

		con := &Content{}

		err = json.Unmarshal([]byte(line), con)
		if err != nil {
			log.Fatalf("json %#v", err)
		}

		log.Printf("%#v", con)
	}
}
