// @APIVersion 1.0.0
// @Title beego Test API
// @Description beego has a very cool tools to autogenerate documents for your API
// @Contact astaxie@gmail.com
// @TermsOfServiceUrl http://beego.me/
// @License Apache 2.0
// @LicenseUrl http://www.apache.org/licenses/LICENSE-2.0.html
package routers

import (
	"github.com/Xe/servapi/controllers"

	"github.com/astaxie/beego"
)

func init() {
	ns := beego.NewNamespace("/v1",
		beego.NSNamespace("/channel",
			beego.NSRouter("/:channel/info", &controllers.ChannelController{}, "get:Info"),
			beego.NSRouter("/:channel/flags", &controllers.ChannelController{}, "get:Flags"),
		),
		beego.NSNamespace("/account",
			beego.NSRouter("/login", &controllers.LoginController{}, "post:Login"),
			beego.NSRouter("/logout", &controllers.LoginController{}, "post:Logout"),
		),
	)
	beego.AddNamespace(ns)
}
