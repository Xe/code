package main

import (
	_ "github.com/Xe/servapi/docs"
	_ "github.com/Xe/servapi/routers"

	"github.com/astaxie/beego"
)

func main() {
	if beego.RunMode == "dev" {
		beego.DirectoryIndex = true
		beego.StaticDir["/swagger"] = "swagger"
	}

	beego.Run()
}
