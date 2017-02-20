# gurren
--
    import "github.com/Xe/gurren/middleware/gurren"

Package gurren is a dead simple negroni middleware that forwards application
logging and metrics data to ElasticSearch.

## Usage

#### type Message

```go
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
```


#### type StatsLog

```go
type StatsLog struct {
}
```

StatsLog is a negroni middleware that will log access and statstics information
to an elasticsearch cluster. Even though Gurren is synchronous, this wrapper is
asynchronous.

#### func  New

```go
func New(urls []string, index string, workers int) (*StatsLog, error)
```
New creates a new StatsLog for use in negroni, or an error if something fails.

#### func (*StatsLog) Log

```go
func (sl *StatsLog) Log(r *http.Request, message string)
```
Log passes string data through to Gurren. This is indended for applications
logging arbitrary additional unformatted text data.

#### func (*StatsLog) ServeHTTP

```go
func (sl *StatsLog) ServeHTTP(rw http.ResponseWriter, r *http.Request, next http.HandlerFunc)
```
ServeHTTP will collect timing and access information for the negroni app and log
it to ElasticSearch.

Most of the work will be done asynchronously with a background worker.

TODO: handle panic()'s
