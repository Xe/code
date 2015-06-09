package main

import (
	"bufio"
	"fmt"
	"net"
	"net/http"
	"net/textproto"
	"os"

	"github.com/Xe/Tetra/1459"
	"github.com/go-martini/martini"
)

type connection struct {
	conn net.Conn
}

func connect(host string) (c *connection) {
	conn, err := net.Dial("tcp", host)
	if err != nil {
		panic(err)
	}

	c = &connection{
		conn: conn,
	}

	return
}

func (c *connection) sendline(line string) {
	fmt.Fprintf(c.conn, "%s\r\n", line)
}

func main() {
	m := martini.Classic()
	conn := connect(os.Getenv("IHOST"))

	m.Get("/", func(req *http.Request) (int, string) {
		username := req.URL.Query()["username"][0]
		conn.sendline("PRIVMSG " + os.Getenv("JOIN") + " :" + username + " sent a yo!")

		return 200, "OK"
	})

	go doBot(conn)

	m.Run()
}

func doBot(c *connection) {
	conn := c.conn
	tp := textproto.NewReader(bufio.NewReader(conn))

	nick := os.Getenv("NICK")
	user := os.Getenv("USER")
	gecos := os.Getenv("GECOS")
	ajoin := os.Getenv("JOIN")

	c.sendline(fmt.Sprintf("NICK %s", nick))
	c.sendline(fmt.Sprintf("USER %s a a :%s", user, gecos))

	for {
		line, err := tp.ReadLine()
		if err != nil {
			break
		}

		if line == "" {
			continue
		}

		fmt.Println(line)

		parsedline := r1459.NewRawLine(line)

		if parsedline.Verb == "001" {
			c.sendline("JOIN " + ajoin)
		} else if parsedline.Verb == "PING" {
			c.sendline("PONG :" + parsedline.Args[0])
		}
	}
}
