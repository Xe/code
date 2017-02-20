package main

import (
	"flag"
	"fmt"
	"net/http"
	"os"
	"runtime"

	"github.com/Xe/hive/common"
	"github.com/Xe/hive/web"
	"github.com/codegangsta/negroni"
)

var (
	u *common.Uplink

	local = flag.Bool("local", false, "serve on port 6670?")
)

func main() {
	flag.Parse()

	if *local {
		n := negroni.Classic()
		n.UseHandlerFunc(handleHello)
		n.Run(":6770")

		os.Exit(0)
	}

	var err error
	u, err = common.Connect()
	if err != nil {
		panic(err)
	}

	_, err = u.QueueSubscribe(
		common.GenerateHTTPChannelName("greedo.xeserv.us:9090", "*", "/"),
		"http",
		web.Wrap(u, http.HandlerFunc(handleHello)),
	)

	runtime.Goexit()
}

func handleHello(rw http.ResponseWriter, req *http.Request) {
	fmt.Fprintf(rw, "Hello world from net/http proxied via hive\n\n")
	if u != nil {
		fmt.Fprintf(rw, "UUID: %s\nReply Channel: %s\n", u.UUID, req.Header.Get("X-Reply-Chan"))
	}
}
