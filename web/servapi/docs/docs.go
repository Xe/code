package docs

import (
	"encoding/json"
	"strings"

	"github.com/astaxie/beego"
	"github.com/astaxie/beego/swagger"
)

const (
    Rootinfo string = `{"apiVersion":"1.0.0","swaggerVersion":"1.2","apis":[{"path":"/channel","description":"Channel/ChanServ operations\n"},{"path":"/account","description":""}],"info":{"title":"beego Test API","description":"beego has a very cool tools to autogenerate documents for your API","contact":"astaxie@gmail.com","termsOfServiceUrl":"http://beego.me/","license":"Url http://www.apache.org/licenses/LICENSE-2.0.html"}}`
    Subapi string = `{"/account":{"apiVersion":"1.0.0","swaggerVersion":"1.2","basePath":"","resourcePath":"/account","produces":["application/json","application/xml","text/plain","text/html"],"apis":[{"path":"/login","description":"","operations":[{"httpMethod":"POST","nickname":"login","type":"","summary":"logs in to the given account with the given password","responseMessages":[{"code":200,"message":"key","responseModel":""},{"code":403,"message":"No such account or incorrect password","responseModel":""}]}]}]},"/channel":{"apiVersion":"1.0.0","swaggerVersion":"1.2","basePath":"","resourcePath":"/channel","produces":["application/json","application/xml","text/plain","text/html"],"apis":[{"path":"/:channel/info","description":"","operations":[{"httpMethod":"GET","nickname":"info","type":"","summary":"get information on a channel","responseMessages":[{"code":200,"message":"atheme.ChannelInfo","responseModel":""},{"code":404,"message":"No such channel","responseModel":""}]}]},{"path":"/:channel/flags","description":"","operations":[{"httpMethod":"GET","nickname":"flags","type":"","summary":"gets the access list of a channel","responseMessages":[{"code":200,"message":"[]atheme.Flagset","responseModel":""},{"code":403,"message":"No permission","responseModel":""}]}]}]}}`
    BasePath string= "/v1"
)

var rootapi swagger.ResourceListing
var apilist map[string]*swagger.ApiDeclaration

func init() {
	err := json.Unmarshal([]byte(Rootinfo), &rootapi)
	if err != nil {
		beego.Error(err)
	}
	err = json.Unmarshal([]byte(Subapi), &apilist)
	if err != nil {
		beego.Error(err)
	}
	beego.GlobalDocApi["Root"] = rootapi
	for k, v := range apilist {
		for i, a := range v.Apis {
			a.Path = urlReplace(k + a.Path)
			v.Apis[i] = a
		}
		v.BasePath = BasePath
		beego.GlobalDocApi[strings.Trim(k, "/")] = v
	}
}


func urlReplace(src string) string {
	pt := strings.Split(src, "/")
	for i, p := range pt {
		if len(p) > 0 {
			if p[0] == ':' {
				pt[i] = "{" + p[1:] + "}"
			} else if p[0] == '?' && p[1] == ':' {
				pt[i] = "{" + p[2:] + "}"
			}
		}
	}
	return strings.Join(pt, "/")
}
