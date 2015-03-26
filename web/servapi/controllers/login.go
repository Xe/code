package controllers

import (
	"log"
	"net/http"

	"github.com/Xe/Tetra/atheme"
	"github.com/astaxie/beego"
)

type LoginController struct {
	beego.Controller
}

// @Title login
// @Description logs in to the given account with the given password
// @Success 200 {string} API key
// @Failure 403 No such account or incorrect password
// @router /login post
func (l *LoginController) Login() {
	l.Ctx.Request.ParseForm()

	username := l.Ctx.Request.Form.Get("username")
	if username == "" {
		l.CustomAbort(http.StatusForbidden, "No such account or incorrect username")
	}

	password := l.Ctx.Request.Form.Get("password")
	if password == "" {
		l.CustomAbort(http.StatusForbidden, "No such account or incorrect password")
	}

	log.Printf("u: %s, p: %s", username, password)

	ath, _ := atheme.NewAtheme(endpoint)

	err := ath.Login(username, password)

	if err != nil {
		l.CustomAbort(http.StatusForbidden, "No such account or incorrect password")
	}

	l.Data = map[interface{}]interface{}{
		"apikey":  ath.Authcookie,
		"account": ath.Account,
		"privs":   ath.GetPrivset(),
	}
	l.ServeJson()
}

func (l *LoginController) Logout() {
	l.Data = map[interface{}]interface{}{
		"status": "okay",
	}

	l.ServeJson()
}
