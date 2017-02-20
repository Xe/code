package main

import (
	"encoding/json"
	"log"
	"net/http"
	"os"

	"github.com/Xe/Tetra/atheme"
	"github.com/codegangsta/negroni"
)

var (
	A *atheme.Atheme
)

type User struct {
	Account string `json:"account"`
	Authkey string `json:"authkey"`
	Uid     string `json:"uid"`
}

func init() {
	url := os.Getenv("ATHEME_URL")
	username := os.Getenv("ATHEME_USERNAME")
	password := os.Getenv("ATHEME_PASSWORD")

	A, _ = atheme.NewAtheme(url)
	if err := A.Login(username, password); err != nil {
		log.Fatal(err)
	}
}

func getuid(account string, recur bool) (uid string, err error) {
	A.Login(os.Getenv("ATHEME_USERNAME"), os.Getenv("ATHEME_PASSWORD"))
	uid, err = A.NickServ.Uid(account)

	return
}

func main() {
	router := http.NewServeMux()

	router.HandleFunc("/login", func(w http.ResponseWriter, req *http.Request) {
		if req.Method != "POST" {
			w.WriteHeader(http.StatusMethodNotAllowed)
			w.Write([]byte(`wrong method`))
			return
		}

		_ = req.ParseForm()

		username := req.Form.Get("username")
		password := req.Form.Get("password")

		user, _ := atheme.NewAtheme(os.Getenv("ATHEME_URL"))
		err := user.Login(username, password)
		if err != nil {
			w.WriteHeader(http.StatusForbidden)
			w.Write([]byte(`Wrong credentials`))
			return
		}

		uid, err := getuid(username, true)
		if err != nil {
			w.WriteHeader(http.StatusForbidden)
			w.Write([]byte(`Wrong credentials`))
			return
		}

		reply, err := json.Marshal(&User{
			Uid:     uid,
			Account: username,
			Authkey: user.Authcookie,
		})

		w.WriteHeader(200)
		w.Write(reply)
	})

	port := os.Getenv("PORT")
	if port == "" {
		port = "5000"
	}

	n := negroni.Classic()
	n.UseHandler(router)

	n.Run(":" + port)
}
