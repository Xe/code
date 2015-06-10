package main

import (
	"encoding/json"
	_ "html/template"
	"io/ioutil"
	"net/http"
	"os"

	_ "github.com/Xe/Tetra/atheme"
	"github.com/Xe/oleg/go" // TODO: Replace with something sane for prod
	"github.com/codegangsta/negroni"
	"github.com/drone/routes"
	"gopkg.in/unrolled/render.v1"
)

var (
	r  *render.Render
	db *oleg.Database
)

func main() {
	r = render.New()
	db = oleg.Purchase("127.0.0.1", "38080")

	mux := routes.New()

	mux.Get("/", serveIndex)
	mux.Get("/request", getVhostRequest)
	mux.Get("/json/:id", getJsonFile)
	mux.Get("/confirm/:id", confirmDomain)

	n := negroni.Classic()

	n.UseHandler(mux)

	var port string = "3000"
	if foo := os.Getenv("PORT"); foo != "" {
		port = foo
	}

	n.Run(":" + port)
}

func serveIndex(w http.ResponseWriter, req *http.Request) {
	r.HTML(w, http.StatusOK, "index", nil)
}

func getJsonFile(w http.ResponseWriter, req *http.Request) {
	params := req.URL.Query()
	id := params.Get(":id")

	toServe, err := db.Unjar("requests", id)
	if err != nil {
		r.HTML(w, 404, "404", nil)
		return
	}

	r.Data(w, http.StatusOK, []byte(toServe))
}

func getVhostRequest(w http.ResponseWriter, req *http.Request) {
	params := req.URL.Query()
	requester := params.Get("requester")
	vhost := params.Get("vhost")

	c := NewConfirmation(vhost, requester)

	out, err := json.Marshal(c)
	if err != nil {
		panic(err)
	}

	db.Jar("requests", c.ID, string(out))

	r.HTML(w, http.StatusOK, "confirmation", c)
}

func confirmDomain(w http.ResponseWriter, req *http.Request) {
	params := req.URL.Query()
	id := params.Get(":id")

	datastr, err := db.Unjar("requests", id)
	if err != nil {
		r.HTML(w, 404, "404", nil)
		return
	}

	var c *Confirmation

	err = json.Unmarshal([]byte(datastr), c)
	if err != nil {
		panic(err)
	}

	resp, err := http.Get("http://" + c.Domain + "/vhost.json")
	if err != nil {
		if resp.StatusCode == http.StatusNotFound {
			r.HTML(w, 500, "failed", "File not found")
			return
		}

		panic(err)
	}

	body, _ := ioutil.ReadAll(resp.Body)
	defer resp.Body.Close()

	var theirdata *Confirmation

	err = json.Unmarshal(body, theirdata)

	if c.Date != theirdata.Date {
		fail(w)
		return
	}

	if c.Domain != theirdata.Domain {
		fail(w)
		return
	}

	if c.ID != theirdata.Domain {
		fail(w)
		return
	}

	if c.Requester != theirdata.Requester {
		fail(w)
		return
	}

	r.HTML(w, 200, "success", c)
}

func fail(w http.ResponseWriter) {
	r.HTML(w, http.StatusBadRequest, "failed", "Data did not match")
}
