import cherrypy

CONFIG_SKEL = """
var params = {};
params.host = '%s';
params.port = 6667;
params.policyport = 8430;
params.language = 'en';
params.realname = '%s';
params.showJoinPartMessages = false;
params.nick = '%s';
params.autojoin = '%s';
params.showServerWindow = true;
params.showNickSelection = true;
params.showIdentitySelection = true;
params.navigationPosition = 'top';
params.showRegisterNicknameButton = true;
"""

HTML = """
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
<head>
 <meta http-equiv="Content-Type" content="text/html; charset=utf8" />
 <title>%s Webchat</title>
 <script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/swfobject/2.2/swfobject.js"></script>
 <script>%s</script>
 <style type="text/css">
    html { height: 100%; overflow: hidden; }
    body { height:100%; margin:0;   padding:0; background-color:#999;   }
 </style>
</head>

<body>
 <div id="lightIRC" style="height:100%; text-align:center;">
  <p><a href="http://www.adobe.com/go/getflashplayer"><img src="http://www.adobe.com/images/shared/download_buttons/get_flash_player.gif" alt="Get Adobe Flash player" /></a></p>
 </div>

 <script type="text/javascript">
    swfobject.embedSWF("static/lightIRC.swf", "lightIRC", "100%", "100%", "10.0.0", "static/expressInstall.swf", params);
 </script>
</body>
</html>
"""

HOST = "irc.yolo-swag.com"
NICK = "user%"
NETNAME = "ShadowNET"
REALNAME = "Webchat user"
DEFCHANNEL = "#niichan"

class Config:
    def __init__(self, host, realname, nick):
        self.config = CONFIG_SKEL % (host, realname, nick, "%%s")

    def seed_channel(self, channelname):
        return self.config % channelname

class ChatPage:
    _cp_config = {'tools.staticdir.on' : True,
            'tools.staticdir.dir' : '/home/xena/code/lightembed/static',
            'tools.staticdir.index': 'index.html'
    }

    def __init__(self):
        self.config = Config(HOST, REALNAME, NICK)

    @cherrypy.expose
    def index(self, channel=None):
        if channel is None:
            channel = DEFCHANNEL

        config = self.config.seed_channel(channel)

        return HTML % (NETNAME, config)

cherrypy.config.update({'server.socket_host': '0.0.0.0',
    'server.socket_port': 9999
})

cherrypy.quickstart(ChatPage())

