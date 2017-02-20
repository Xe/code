package oleg

import (
	"bytes"
	"errors"
	"fmt"
	"io/ioutil"
	"net/http"
	"time"
)

// Our errors
var (
	// Key not found in OlegDB
	ErrNoSuchKey = errors.New("No such key")
)

// Database is an OlegDB reference. This is good for data storage I think.
type Database struct {
	host string
	port string
}

// Purchase creates a new Database by getting it from the store.
//
// The hostname and port arguments are strings because laziness is next to godliness.
func Purchase(host, port string) *Database {
	return &Database{
		host: host,
		port: port,
	}
}

func (d *Database) request(table, key, value, method string, expiry time.Time) (res *http.Response, err error) {
	client := &http.Client{}

	var req *http.Request

	if method == "POST" && value != "" {
		req, err = http.NewRequest("POST", "http://"+d.host+":"+d.port+"/"+table+"/"+key, bytes.NewBuffer([]byte(value)))
	} else {
		req, err = http.NewRequest(method, "http://"+d.host+":"+d.port+"/"+table+"/"+key, nil)
	}

	if err != nil {
		return nil, err
	}

	if expiry != time.Unix(0, 0) {
		req.Header = map[string][]string{
			"X-OlegDB-use-by": {fmt.Sprintf("%d", expiry.Unix())},
		}
	}

	res, err = client.Do(req)

	if err != nil {
		return nil, err
	}

	return
}

// Jar puts key into table in OlegDB, returning an error if the operation failed.
//
// There's plenty of reasons this could fail, but none of them would ever need to come up.
func (d *Database) Jar(table, key, value string) error {
	_, err := d.request(table, key, value, "POST", time.Unix(0, 0))
	return err
}

// Unjar gets key from table out of OlegDB, returning an error if the operation failed.
func (d *Database) Unjar(table, key string) (value string, err error) {
	res, err := d.request(table, key, "", "GET", time.Unix(0, 0))

	defer res.Body.Close()
	switch res.StatusCode {
	case http.StatusNotFound:
		return "", ErrNoSuchKey
	}

	body, _ := ioutil.ReadAll(res.Body)

	value = string(body)

	return
}

// Scoop removes key from table. Get that crap out of the mayo jar.
func (d *Database) Scoop(table, key string) (err error) {
	_, err = d.request(table, key, "", "DELETE", time.Unix(0, 0))
	return err
}
