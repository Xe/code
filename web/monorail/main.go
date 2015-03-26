package main

import (
	"github.com/go-martini/martini"
	"github.com/martini-contrib/binding"
	"github.com/martini-contrib/gzip"
	"github.com/martini-contrib/render"
	"github.com/martini-contrib/sessions"
	"github.com/martini-contrib/strict"

	"github.com/Xe/monorail/models/forms"
	"github.com/Xe/monorail/views/index"
	"github.com/Xe/monorail/views/login"
)

func main() {
	m := martini.Classic()
	store := sessions.NewCookieStore([]byte("secret123"))

	// Use a sane set of useful middlewares
	// gzip     - bandwidth saving
	// sessions - http sessions via cookies
	// strict   - error pages on 404, etc
	// static   - add a more sane static folder name
	// render   - automatic template management
	m.Use(gzip.All())
	m.Use(sessions.Sessions("monorail", store))
	m.Use(strict.Strict)
	m.Use(martini.Static("static"))
	m.Use(render.Renderer(render.Options{
		Directory:  "templates",
		Extensions: []string{".tmpl", ".html"},
		Layout:     "layout",
	}))

	m.Get("/", index.RenderIndex)

	m.Get("/login", login.RenderLoginGET)
	m.Post("/login", binding.Bind(forms.LoginForm{}), login.RenderLoginPOST)

	m.Router.NotFound(strict.MethodNotAllowed, strict.NotFound)

	m.Run()
}
