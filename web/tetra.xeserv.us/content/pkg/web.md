+++
title = "web"
+++

```go
import "github.com/Xe/Tetra/bot/web"
```

Package web is a series of negroni middlewares for Tetra.

## Usage

#### type Logger

```go
type Logger struct {
	// Logger inherits from log.Logger used to log messages with the Logger middleware
	*log.Logger
}
```

Logger is a middleware handler that logs the request as it goes in and the
response as it goes out.

#### func  NewLogger

```go
func NewLogger() *Logger
```
NewLogger returns a new Logger instance

#### func (*Logger) ServeHTTP

```go
func (l *Logger) ServeHTTP(rw http.ResponseWriter, r *http.Request, next http.HandlerFunc)
```
