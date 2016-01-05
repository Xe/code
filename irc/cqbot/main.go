package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"runtime"
	"strings"

	"github.com/Xe/cqbot/bot"
	"github.com/codegangsta/cli"
	"github.com/stevedonovan/luar"
)

func main() {
	app := cli.NewApp()
	app.Name = "cqbot"
	app.Usage = "Kicks off the cqbot irc bot"
	app.Version = "0.1-dev"
	app.Author = "Christine Dodrill <xena@yolo-swag.com>"

	app.Flags = []cli.Flag{
		cli.StringFlag{"conf, c", "cqbot.conf", "Configuration file to use", "CQBOT_CONF"},
	}

	app.Action = startBot

	app.Commands = []cli.Command{
		{
			Name:        "run",
			Usage:       "Kickstart the bot",
			Action:      startBot,
			Description: "Start the bot and its associated goroutines. If no other action is given, this will start.",
		},
		{
			Name:  "genconf",
			Usage: "Generate an example configuration file",
			Flags: []cli.Flag{
				cli.StringFlag{"name, n", "cqbot.conf", "Filename to write example configuration to.", "GENCONF"},
			},
			Action: func(c *cli.Context) {
				conf := cqbot.LoadConfig("cqbot.conf.example")
				conf.Export(c.String("name"))
				fmt.Printf("Configuration written to %s\n", c.String("name"))
			},
			Description: "Generates a configuration file in the correct place with known sane settings.",
		},
	}

	app.Run(os.Args)
}

func startBot(c *cli.Context) {
	runtime.GOMAXPROCS(runtime.NumCPU())

	var confname string

	if os.Getenv("DOCKER") == "YES" {
		confname = os.Getenv("CQBOT_CONF")
		fmt.Printf("Running in docker with config %s\n", confname)
	} else {
		confname = c.String("conf")
		fmt.Printf("Using config file %s\n", confname)
	}

	bot := cqbot.NewBot(confname)

	bot.Log.Printf("Initializing...")

	bot.AddCommand("MEM", "Shows memory statistics", 0, 0,
		func(user *cqbot.User, message []string) string {
			stats := new(runtime.MemStats)
			runtime.ReadMemStats(stats)

			return fmt.Sprintf("Allocs: %d, Frees: %d, Bytes in use: %d, Scripts loaded: %d",
				stats.Mallocs, stats.Frees, stats.Alloc, len(bot.Scripts))
		})

	files, _ := ioutil.ReadDir("./scripts")

	for _, f := range files {
		switch mode := f.Mode(); {
		case mode.IsDir():
			continue
		case mode.IsRegular():
			go func(file os.FileInfo) {
				err := bot.LoadScript(file.Name())
				if err != nil {
					bot.Log.Printf("Unable to load %s!", file.Name())
				}
			}(f)
		}
	}

	bot.Connect()

	bot.HandleCommands()
}
