gurren
======

Gurren is a simple logging library for Go programs that sends all its logs to Elasticsearch. Make your logs the logs that will record the heavens!

![](https://i.imgur.com/irtxk6a.png)

Usage
-----

### Standalone

```go
package main

import (
	"github.com/Xe/gurren"
)

func main() {
	l, err := gurren.New([]string{"http://127.0.0.1:9200"}, "logs")
	if err != nil {
		panic(err)
	}

	err = l.Log("hi mom")
	if err != nil {
		panic(err)
	}
}
```

### Inside Negroni

```go
package main

import (
	"net/http"

	"github.com/Xe/gurren/middleware/gurren"
	"github.com/codegangsta/negroni"
	"github.com/pilu/xrequestid"
)

func main() {
	sl, err := gurren.New([]string{"http://127.0.0.1:9200"}, "test", 1)
	if err != nil {
		panic(err)
	}

	mux := http.NewServeMux()

	mux.HandleFunc("/", func(rw http.ResponseWriter, r *http.Request) {
		rw.Write([]byte("Hi there from static replies!"))
	})

	n := negroni.Classic()

	n.Use(xrequestid.New(16))
	n.Use(sl)
	n.UseHandler(mux)

	n.Run(":3000")
}
```

It will create output that looks something like this:

![](http://puu.sh/jaJ1c/34ed7ee150.png)
