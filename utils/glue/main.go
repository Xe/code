/*
Command glue is glue between Go and Lua.
*/
package main

import (
	"flag"
	"fmt"
	"log"
	"os"

	"code.google.com/p/go-uuid/uuid"
	"github.com/Bowery/prompt"
	"github.com/Xe/glue/script"
	"github.com/stevedonovan/luar"
)

var (
	follow  = flag.Bool("f", false, "continue in repl at end of script execute")
	version = flag.Bool("version", false, "show version and exit")
)

func showVersion() {
	fmt.Println("Glue version 0.1")
}

func copyright() {
	fmt.Println("Copyright Xena, 2015")
	fmt.Println("This code is licensed under the terms of the Zlib license.")
}

func main() {
	flag.Parse()

	if *version {
		showVersion()
		copyright()

		os.Exit(0)
	}

	var s *script.Script
	var sname string
	var err error

	args := flag.Args()

	if len(args) == 0 {
		s = &script.Script{
			Name: "<repl>",
			L:    luar.Init(),
			Log:  log.New(os.Stdout, "", log.LstdFlags),
			Uuid: uuid.New(),
		}
		s.Seed()

		goto repl
	}

	sname = args[0]

	s, err = script.NewScript(sname)
	if err != nil {
		log.Fatal(err)
	}

	if *follow {
		goto repl
	}

	return

repl:
	defer s.L.Close()

	if s.Name == "<repl>" {
		showVersion()
		copyright()
	}
	var history []string = make([]string, 0, 10)

	term, err := prompt.NewTerminal()
	if err != nil {
		log.Fatal(err)
	}
	defer term.Close()

	term.History = history

	for {
		line, err := term.Basic("glue> ", true)
		if err != nil {
			switch err {
			default:
				log.Printf("go error: %s", err.Error())
				goto goodbye
			case prompt.ErrEOF:
				goto goodbye
			case prompt.ErrCTRLC:
				goto goodbye
			}
		}

		if line == "exit" {
			break
		}

		term.Close()
		err = s.L.DoString(line)
		if err != nil {
			log.Printf("lua error: %s", err.Error())
		}

		term, err = prompt.NewTerminal()
		if err != nil {
			log.Fatal(err)
		}

		term.History = history

	}

goodbye:
}
