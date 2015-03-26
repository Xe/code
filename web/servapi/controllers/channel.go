package controllers

import (
	"net/http"

	"github.com/astaxie/beego"
)

// Channel/ChanServ operations
type ChannelController struct {
	beego.Controller
}

// @Title info
// @Description get information on a channel
// @Success 200 atheme.ChannelInfo
// @Failure 404 No such channel
// @router /:channel/info get
func (c *ChannelController) Info() {
	channel := c.Ctx.Input.Params[":channel"]

	info, err := a.ChanServ.Info("#" + channel)
	if err != nil {
		c.CustomAbort(http.StatusNotFound, "Channel #"+channel+" not found")
		return
	}

	c.Data["json"] = info
	c.ServeJson()
}

// @Title flags
// @Description gets the access list of a channel
// @Success 200 []atheme.Flagset
// @Failure 403 No permission
// @router /:channel/flags get
func (c *ChannelController) Flags() {
	channel := c.Ctx.Input.Params[":channel"]

	flags, err := a.ChanServ.GetAccessList("#" + channel)
	if err != nil {
		c.CustomAbort(http.StatusForbidden, "You do not have permission to see the flags for #"+channel)
		return
	}

	c.Data["json"] = flags
	c.ServeJson()
}
