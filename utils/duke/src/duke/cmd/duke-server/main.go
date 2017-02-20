package main

import (
	"duke"
	"duke/config"
	"flag"
	"fmt"
	"io/ioutil"
	"net/http"

	"github.com/boltdb/bolt"
	"github.com/codegangsta/negroni"
	"github.com/djherbis/stow"
	"github.com/scalingdata/gcfg"
)

var (
	cfgFile = flag.String("conf", "./etc/duke.ini", "config file location")

	conf *config.Config
	db   *stow.Store
)

func main() {
	flag.Parse()

	conf = new(config.Config)

	err := gcfg.ReadFileInto(conf, *cfgFile)
	if err != nil {
		panic(err)
	}

	bdb, err := bolt.Open(conf.Server.DataLocation, 0600, nil)
	if err != nil {
		panic(err)
	}
	defer bdb.Close()

	switch conf.Server.Format {
	case "xml":
		db = stow.NewXMLStore(bdb, []byte("duke-xml"))
	case "json":
		db = stow.NewJSONStore(bdb, []byte("duke-json"))
	case "gob":
		db = stow.NewStore(bdb, []byte("duke-gob"))
	default:
		panic("no set data store in " + *cfgFile)
	}

	db.Put([]byte("foo"), "bar")

	mux := http.NewServeMux()

	mux.HandleFunc("/lookup", func(rw http.ResponseWriter, r *http.Request) {
		if r.Method != "GET" {
			rw.WriteHeader(http.StatusNotAcceptable)
			fmt.Fprintf(rw, "Wrong method")
			return
		}

		user := r.URL.Query().Get("user")

		if len(user) == 0 {
			rw.WriteHeader(http.StatusNotAcceptable)
			fmt.Fprintf(rw, "Need user parameter")
			return
		}

		rw.WriteHeader(http.StatusOK)

		db.ForEach(func(key *duke.Key) {
			if key.Username == user {
				rw.Write([]byte(fmt.Sprintf("# for %s with fp %s\n", key.Username, key.Fingerprint)))
				rw.Write([]byte(key.String()))
				rw.Write([]byte("\n"))
			}
		})
	})

	mux.HandleFunc("/add", func(rw http.ResponseWriter, r *http.Request) {
		if r.Method != "POST" {
			rw.WriteHeader(http.StatusNotAcceptable)
			fmt.Fprintf(rw, "Wrong method")
			return
		}

		user := r.URL.Query().Get("user")

		if len(user) == 0 {
			rw.WriteHeader(http.StatusNotAcceptable)
			fmt.Fprintf(rw, "Need user parameter")
			return
		}

		body, err := ioutil.ReadAll(r.Body)
		if err != nil {
			panic(err) // should never happen
		}

		k := duke.NewKey(user, string(body))

		err = db.Put([]byte(k.Fingerprint), k)
		if err != nil {
			panic(err)
		}

		rw.WriteHeader(http.StatusOK)
	})

	mux.HandleFunc("/remove", func(rw http.ResponseWriter, r *http.Request) {
		if r.Method != "DELETE" {
			rw.WriteHeader(http.StatusNotAcceptable)
			fmt.Fprintf(rw, "Wrong method")
			return
		}

		fingerprint := r.URL.Query().Get("fingerprint")

		if len(fingerprint) == 0 {
			rw.WriteHeader(http.StatusNotAcceptable)
			fmt.Fprintf(rw, "Need fingerprint parameter")
			return
		}

		var k = new(duke.Key)

		err := db.Pull([]byte(fingerprint), k)
		if err != nil {
			panic(err)
		}

		rw.WriteHeader(http.StatusOK)
		fmt.Fprintf(rw, "Key with FP %s for user %s deleted.", k.Fingerprint, k.Username)
	})

	n := negroni.Classic()
	n.UseHandler(mux)
	n.Run(conf.Server.Listen)
}
