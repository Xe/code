/*
Command mephiles is an RFC 1459 compliant IRC server.
*/
package main

import (
	"log"
	"os"

	"github.com/Xe/mephiles/server"
	"gopkg.in/alecthomas/kingpin.v1"
)

var (
	app         = kingpin.New("mephiles", "an IRC daemon for the cloud")
	etcdmachine = app.Flag("etcd-machine", "the uplink machine for etcd").Default("http://127.0.0.1:4001").String()

	serve            = app.Command("serve", "kick off irc daemon")
	servername       = serve.Flag("hostname", "hostname to use for ircd, defaulted to machine hostname").String() // de-facto set by default
	serverdesc       = serve.Flag("description", "server description").Default("Unconfigured IRC server").String()
	serverconfigfile = serve.Flag("conf", "config file").Default("etc/config.yaml").String()
)

// `bans` subcommand
/*var (
	banscmd = app.Command("bans", "show or modify bans")

	bansaddcmd       = banscmd.Command("add", "add a network ban")
	bansaddcmdmask   = bansaddcmd.Arg("mask", "user@host mask to ban").Required().String()
	bansaddcmdreason = bansaddcmd.Arg("reason", "reason for the ban").Required().String()
	bansaddcmdtime   = bansaddcmd.Flag("time", "time for the ban").Default("1w").String()
)*/

// `config` subcommand
var (
	configcmd = app.Command("config", "manages ircd configuration")

	configconvert     = configcmd.Command("convert", "convert old configuration to new etcd format")
	configconvertconf = configconvert.Arg("input", "old ircd.conf to convert").Default("ircd.conf").String()

	configdump    = configcmd.Command("dump", "dumps current config to a yaml file or stdout")
	configdumpout = configdump.Arg("output", "output file to use").File()

	configrestore      = configcmd.Command("restore", "restores ircd config from a yaml file or stdin")
	configrestoreinput = configrestore.Arg("input", "input file to read from").ExistingFile()

	configseed                 = configcmd.Command("seed", "seed a sane default configuration")
	configseedyesiwanttodothis = configseed.Flag("yes-i-want-to-do-this", "confirmation flag for seeding config").Required().Bool()
)

func main() {
	command := kingpin.MustParse(app.Parse(os.Args[1:]))

	if envvar := os.Getenv("MEPHILES_ETCD_MACHINE"); envvar != "" {
		etcdmachine = &envvar
	}

	switch command {
	case serve.FullCommand():
		if *servername == "" {
			if name, err := os.Hostname(); err == nil {
				*servername = name
			} else {
				log.Fatal(err)
			}
		}

		server.Serve(*serverconfigfile, *etcdmachine)
	default:
		app.Usage(os.Stderr)

		os.Exit(1)
	}
}
