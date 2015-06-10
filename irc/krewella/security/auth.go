package security

import (
	"net/http"
)

// Struct Auth is a negroni middleware for checking X-Lagann-Auth for the correct uuid
type Auth struct {
	key        string
	HeaderName string
}

// NewAuth returns a new Auth instance for checking against a given API key.
func NewAuth(apikey string) *Auth {
	a := &Auth{
		HeaderName: "X-Krewella-Auth",
		key:        apikey,
	}

	return a
}

func (a *Auth) ServeHTTP(rw http.ResponseWriter, r *http.Request, next http.HandlerFunc) {
	authkey := r.Header.Get(a.HeaderName)
	if authkey == "" || authkey != a.key {
		http.Error(rw, "Wrong or bad "+a.HeaderName, http.StatusUnauthorized)
		return
	}

	next(rw, r)
}
