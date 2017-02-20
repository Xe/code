package main

import (
	"net/http"

	"github.com/Xe/gurren/middleware/gurren"
	"github.com/Xe/middleware"
	"github.com/codegangsta/negroni"
)

func main() {
	sl, err := gurren.New([]string{"http://127.0.0.1:9200"}, "test", 1)
	if err != nil {
		panic(err)
	}

	mux := http.NewServeMux()

	mux.HandleFunc("/", func(rw http.ResponseWriter, r *http.Request) {
		sl.Log(r, "/: Additional logging data")
		rw.Write([]byte("Hi there from static replies!"))
	})

	n := negroni.Classic()

	middleware.Inject(n)

	n.Use(sl)
	n.UseHandler(mux)

	n.Run(":3000")
}
