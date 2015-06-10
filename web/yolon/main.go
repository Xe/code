package main

import (
	"github.com/Xe/oleg/go"
	"github.com/Xe/uuid"
	"github.com/codegangsta/negroni"
	"github.com/drone/routes"

	"encoding/base64"
	"encoding/json"
	"io/ioutil"
	"net/http"
	"os"
)

var (
	OlegHost = "127.0.0.1"
	OlegPort = "38080"

	DB *oleg.Database
)

// UploadImage lets you upload an image and will reply with a
// json object representing the uuid of the image uploaded or
// an error object telling you why you failed.
func UploadImage(w http.ResponseWriter, req *http.Request) {
	// 10 MB
	err := req.ParseMultipartForm(10 * 1024 * 1024)
	if err != nil {
		w.WriteHeader(http.StatusNotAcceptable)
		w.Write([]byte("File too big."))
		return
	}

	m := req.MultipartForm
	files := m.File["upload"]

	for _, file := range files {
		fin, err := file.Open()
		if err != nil {
			w.WriteHeader(http.StatusNotAcceptable)
			w.Write([]byte("File too big."))
			return
		}
		defer fin.Close()

		reply, err := ioutil.ReadAll(fin)
		if err != nil {
			w.WriteHeader(http.StatusNotAcceptable)
			w.Write([]byte(err.Error()))
			return
		}

		id := uuid.NewRandom()
		data := base64.StdEncoding.EncodeToString(reply)

		err = DB.Jar("images", id.String(), data)
		if err != nil {
			w.WriteHeader(http.StatusNotAcceptable)
			w.Write([]byte(err.Error()))
			return
		}

		out, err := json.Marshal(map[string]string{
			"id": id.String(),
		})
		if err != nil {
			panic(err)
		}

		w.Write(out)
	}
}

// GetImage gets an image from the database.
func GetImage(w http.ResponseWriter, req *http.Request) {
	params := req.URL.Query()
	iid := params.Get(":id")

	ibase64, err := DB.Unjar("images", iid)
	if err != nil {
		w.WriteHeader(http.StatusNotFound)
		w.Write([]byte(err.Error()))
		return
	}

	data, err := base64.StdEncoding.DecodeString(ibase64)
	if err != nil {
		w.WriteHeader(http.StatusNotFound)
		w.Write([]byte(err.Error()))
		return
	}

	w.Write(data)
}

func init() {
	if foo := os.Getenv("OLEGDB_PORT_38080_TCP_ADDR"); foo != "" {
		OlegHost = os.Getenv("OLEGDB_PORT_38080_TCP_ADDR")
		OlegPort = os.Getenv("OLEGDB_PORT_38080_TCP_PORT")
	}
}

func main() {
	mux := routes.New()
	DB = oleg.Purchase(OlegHost, OlegPort)

	// Image uploading
	mux.Post("/upload", UploadImage)
	mux.Put("/upload", UploadImage)

	// Image getting
	mux.Get("/image/:id", GetImage)

	mux.Get("/", func(w http.ResponseWriter, req *http.Request) {
		w.Write([]byte(`<!DOCTYPE html>
<html lang="en">
  <head>
    <title>File Upload Demo</title>
  </head>
  <body>
    <div class="container">
      <h1>File Upload Demo</h1>
      <form class="form-signin" method="post" action="/upload" enctype="multipart/form-data">
          <fieldset>
            <input type="file" name="upload" id="upload" multiple="multiple">
            <input type="submit" name="submit" value="Submit">
        </fieldset>
      </form>
    </div>
  </body>
</html>`))
	})

	n := negroni.Classic()
	n.UseHandler(mux)

	port := "3000"
	if foo := os.Getenv("PORT"); foo != "" {
		port = os.Getenv("PORT")
	}

	n.Run(":" + port)
}
