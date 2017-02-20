package web

import (
	"bytes"
	"encoding/gob"
	"io/ioutil"
	"net/http"
	"net/url"
	"strings"

	"github.com/Xe/hive/common"
)

func init() {
	gob.Register(&Request{})
}

type Request struct {
	URL        string         `json:"url"`
	VHost      string         `json:"vhost"`
	Headers    http.Header    `json:"headers"`
	Body       string         `json:"body"`
	Method     string         `json:"method"`
	Cookies    []*http.Cookie `json:"cookies"`
	RemoteAddr string         `json:"remoteaddr"`
	RequestURI string         `json:"requesturi"`
}

func Convert(req *http.Request) (*Request, error) {
	body, err := ioutil.ReadAll(req.Body)
	if err != nil {
		return nil, err
	}
	defer req.Body.Close()

	result := &Request{
		URL:        req.URL.String(),
		VHost:      strings.Replace(req.Host, ".", "%2e", -1),
		Headers:    req.Header,
		Body:       string(body),
		Method:     req.Method,
		Cookies:    req.Cookies(),
		RemoteAddr: req.RemoteAddr,
		RequestURI: req.RequestURI,
	}

	if result.VHost == "" {
		result.VHost = "localhost"
	}

	return result, nil
}

func (req *Request) Host() string {
	return strings.Replace(req.VHost, "%2e", ".", -1)
}

func (r *Request) Convert() *http.Request {
	body := ioutil.NopCloser(bytes.NewBuffer([]byte(r.Body)))
	result := &http.Request{
		Method:        r.Method,
		Proto:         "HTTP/1.1",
		ProtoMajor:    1,
		ProtoMinor:    1,
		Header:        r.Headers,
		Body:          body,
		ContentLength: int64(len(r.Body)),
		Host:          r.VHost,
		RemoteAddr:    r.RemoteAddr,
		RequestURI:    r.RequestURI,
	}

	result.URL, _ = url.Parse(r.RequestURI)

	return result
}

func Wrap(u *common.Uplink, h http.Handler) func(string, string, *Request) {
	return func(subject, reply string, req *Request) {
		go u.Watchdog.Feed()

		rw := &Response{}
		rw.Buffer = &bytes.Buffer{}

		req.Headers.Set("X-Reply-Chan", reply)

		h.ServeHTTP(rw, req.Convert())

		body, _ := ioutil.ReadAll(rw.Buffer)
		rw.Body = string(body)

		if rw.StatusCode == 0 {
			rw.WriteHeader(200)
		}

		//u.Printf("%s %s - %d %d", req.Method, req.URL, rw.StatusCode, len(rw.Body))

		err := u.Publish(reply, rw)
		if err != nil {
			panic(err)
		}
	}
}
