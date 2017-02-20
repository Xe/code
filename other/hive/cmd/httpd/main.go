package main

import (
	"flag"
	"log"
	"net/http"
	_ "net/http/pprof"
	"time"

	"github.com/Xe/hive/common"
	"github.com/Xe/hive/web"
	"github.com/codegangsta/negroni"
)

var (
	u          *common.Uplink
	portString = flag.String("port", ":9090", "port/host to listen on")
)

func main() {
	flag.Parse()

	go func() {
		log.Println(http.ListenAndServe("localhost:6060", nil))
	}()

	var err error
	u, err = common.Connect()
	if err != nil {
		panic(err)
	}

	mux := http.NewServeMux()
	mux.HandleFunc("/", handleRouting)

	n := negroni.New()
	n.UseHandler(mux)

	n.Run(*portString)
}

func handleRouting(rw http.ResponseWriter, req *http.Request) {
	hreq, err := web.Convert(req)
	if err != nil {
		panic(err)
	}

	channelName := common.GenerateHTTPChannelName(hreq.VHost, req.Method, req.URL.Path)

	reply := &web.Response{}
	err = u.Request(channelName, hreq, reply, 5*time.Second)
	if err != nil {
		panic(err)
	}

	if reply.Headers != nil {
		for header, contents := range reply.Headers {
			for _, content := range contents {
				rw.Header().Set(header, content)
			}
		}
	}

	if reply.StatusCode == 0 {
		reply.StatusCode = 200
	}

	rw.WriteHeader(reply.StatusCode)

	count, err := rw.Write([]byte(reply.Body))
	if count != len(reply.Body) {
		log.Panicf(
			"Expected to send %d bytes but asked to send %d bytes.\n\nBody: %s",
			len(reply.Body), count, reply.Body,
		)
	}
}
