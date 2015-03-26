package index

import (
	"github.com/martini-contrib/render"
	"github.com/martini-contrib/sessions"
)

func RenderIndex(r render.Render, s sessions.Session) {
	username := s.Get("username")
	var userstring string

	if username == nil {
		userstring = "Unknown user"
	} else {
		userstring = username.(string)
	}

	r.HTML(200, "index/main", userstring)
}
