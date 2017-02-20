package gurren

import (
	"net/http"
	"time"

	glog "github.com/Xe/gurren"
	"github.com/codegangsta/negroni"
)

type Message struct {
	ResponseTime int64  `json:"responsetime,omitempty"`
	Path         string `json:"path"`
	Method       string `json:"method"`
	Remote       string `json:"remote"`
	Status       int    `json:"status,omitempty"`
	StatusText   string `json:"status_text,omitempty"`
	ReqID        string `json:"reqid"`
	Size         int    `json:"size"`
	Finished     bool   `json:"finished"`
	Vhost        string `json:"vhost"`
}

// StatsLog is a negroni middleware that will log access and statstics information
// to an elasticsearch cluster. Even though Gurren is synchronous, this wrapper is
// asynchronous.
type StatsLog struct {
	channel chan interface{}
	urls    []string
	index   string
	l       *glog.Logger
}

// process logs an individual log message in the channel and sends it off.
func (sl *StatsLog) process() {
	for msg := range sl.channel {
		sl.l.Log(msg)
	}
}

// ServeHTTP will collect timing and access information for the negroni app and log
// it to ElasticSearch.
//
// Most of the work will be done asynchronously with a background worker.
//
// TODO: handle panic()'s
func (sl *StatsLog) ServeHTTP(rw http.ResponseWriter, r *http.Request, next http.HandlerFunc) {
	start := time.Now()

	// Make the initial message to log
	m := &Message{
		Path:     r.RequestURI,
		Method:   r.Method,
		Remote:   r.RemoteAddr,
		Finished: false,
		ReqID:    r.Header.Get("X-Request-Id"),
		Vhost:    r.Host,
	}

	// Fire it off
	sl.channel <- m

	next(rw, r)

	// Calculate how long it took
	latency := time.Since(start)
	res := rw.(negroni.ResponseWriter)

	// Get the final logging message recorded
	m = &Message{
		Path:         r.RequestURI,
		Method:       r.Method,
		Remote:       r.RemoteAddr,
		ResponseTime: latency.Nanoseconds(),
		StatusText:   http.StatusText(res.Status()),
		Status:       res.Status(),
		Size:         res.Size(),
		Finished:     true,
		ReqID:        r.Header.Get("X-Request-Id"),
		Vhost:        r.Host,
	}

	// Fire it off
	sl.channel <- m
}

// Log passes string data through to Gurren. This is indended for applications logging arbitrary
// additional unformatted text data.
func (sl *StatsLog) Log(r *http.Request, message string) {
	reqID := r.Header.Get("X-Request-Id")

	proxyFor := r.Header.Get("X-Forwarded-For")
	if proxyFor == "" {
		proxyFor = r.RemoteAddr
	}

	sl.channel <- map[string]interface{}{
		"message": message,
		"reqid":   reqID,
		"path":    r.RequestURI,
		"remote":  proxyFor,
		"kind":    "unformatted",
	}
}

// New creates a new StatsLog for use in negroni, or an error if something fails.
func New(urls []string, index string, workers int) (*StatsLog, error) {
	l, err := glog.New(urls, index)
	if err != nil {
		return nil, err
	}

	sl := &StatsLog{
		channel: make(chan interface{}, workers*50),
		urls:    urls,
		index:   index,
		l:       l,
	}

	for i := 0; i < workers; i++ {
		go sl.process()
	}

	return sl, nil
}
