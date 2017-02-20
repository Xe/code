package main

import (
	"net/http"
	"runtime"

	"github.com/Xe/gurren/middleware/gurren"
	"github.com/Xe/middleware"
	"github.com/codegangsta/negroni"
	"github.com/drone/routes"
	"github.com/yosssi/ace"
)

func main() {
	sl, err := gurren.New([]string{"http://127.0.0.1:9200"}, "test", runtime.NumCPU())
	if err != nil {
		panic(err)
	}

	mux := routes.New()

	// Do handling here

	mux.Get("/", func(rw http.ResponseWriter, r *http.Request) {
		tpl, err := ace.Load("views/layout", "views/index", nil)
		if err != nil {
			http.Error(rw, err.Error(), http.StatusInternalServerError)
			return
		}

		if err := tpl.Execute(rw, nil); err != nil {
			http.Error(rw, err.Error(), http.StatusInternalServerError)
			return
		}
	})

	n := negroni.Classic()

	middleware.Inject(n)
	n.Use(sl)
	n.UseHandler(mux)

	n.Run(":3000")
}
