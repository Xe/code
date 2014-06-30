package main

import (
	"fmt"
	"os"
	"github.com/codegangsta/cli"
	"code.google.com/p/gompd/mpd"
)

func main() {
	app := cli.NewApp()
	app.Name = "boom"
	app.Usage = "make an explosive entrance"

	app.Flags = []cli.Flag {
		cli.StringFlag{"server, s", "127.0.0.1", "MPD server"},
		cli.StringFlag{"port, p", "6600", "MPD port"},
	}

	app.Action = func(c *cli.Context) {
		client, _ := mpd.Dial("tcp", c.String("server") + ":" + c.String("port"))
		attrs, err := client.CurrentSong()
		if err != nil {
			panic(err)
		}

		fmt.Printf("Listening to %s by %s\n",
			attrs["Title"], attrs["Artist"])
	}

	app.Run(os.Args)
}

