package login

import (
	"github.com/Xe/monorail/models/forms"
	"github.com/martini-contrib/render"
	"github.com/martini-contrib/sessions"
)

func RenderLoginGET(r render.Render) {
	r.HTML(200, "login/main", nil)
}

func RenderLoginPOST(r render.Render, l forms.LoginForm, s sessions.Session) {
	s.Set("username", l.Username)

	r.Redirect("/")
}
