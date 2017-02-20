package main

import (
	"duke/config"
	"flag"
	"io"
	"net/http"
	"os"

	"github.com/scalingdata/gcfg"
)

var (
	cfgFile = flag.String("conf", "./etc/duke.ini", "config file location")

	conf *config.Config
)

func main() {
	flag.Parse()

	conf = new(config.Config)

	err := gcfg.ReadFileInto(conf, *cfgFile)
	if err != nil {
		panic(err)
	}

	resp, err := http.Get(conf.Client.URL + "/lookup?user=" + flag.Arg(0))
	defer resp.Body.Close()

	_, err = io.Copy(os.Stdout, resp.Body)
	if err != nil {
		panic(err)
	}
}
