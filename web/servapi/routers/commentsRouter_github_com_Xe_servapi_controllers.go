package routers

import (
	"github.com/astaxie/beego"
)

func init() {
	
	beego.GlobalControllerRouter["github.com/Xe/servapi/controllers:LoginController"] = append(beego.GlobalControllerRouter["github.com/Xe/servapi/controllers:LoginController"],
		beego.ControllerComments{
			"Login",
			`/login`,
			[]string{"post"},
			nil})

	beego.GlobalControllerRouter["github.com/Xe/servapi/controllers:ChannelController"] = append(beego.GlobalControllerRouter["github.com/Xe/servapi/controllers:ChannelController"],
		beego.ControllerComments{
			"Info",
			`/:channel/info`,
			[]string{"get"},
			nil})

	beego.GlobalControllerRouter["github.com/Xe/servapi/controllers:ChannelController"] = append(beego.GlobalControllerRouter["github.com/Xe/servapi/controllers:ChannelController"],
		beego.ControllerComments{
			"Flags",
			`/:channel/flags`,
			[]string{"get"},
			nil})

}
