package web

import (
	"bytes"
	"encoding/gob"
	"net/http"
)

func init() {
	gob.Register(&Response{})
}

// Response is the data type used from worker -> endpoint
type Response struct {
	*bytes.Buffer
	Body       string      `json:"body"`
	Headers    http.Header `json:"headers"`
	StatusCode int         `json:"status"`
}

// Header returns the headers of the recieved Response.
func (r *Response) Header() http.Header {
	return r.Headers
}

// WriteHeader sets the HTTP status code of a Response.
func (r *Response) WriteHeader(code int) {
	r.StatusCode = code
}
