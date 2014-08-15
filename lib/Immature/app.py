import web
from immature import immature

urls = (
	"/", "index",
	"/favicon.ico", "index",
	"/pants.html", "html",
	"/pants.txt", "html",
	"/pants", "html"
)

class index:
	def GET(self):
		return ""

def shuck(string):
	return string[1:-1]
	
def xss(string):
	if "<" in string:
		return True

class html:
	def GET(self):
		tweet = web.input(name = "text")
		spray = str(tweet["text"])
		shucked = False
		
		if spray.startswith("\""):
			spray = shuck(spray)
			shucked = True
			
		print spray
		
		if shucked:
			return "\"" + pants.immature(spray) + "\""
		else:
			return pants.immature(spray)
		

app = web.application(urls, globals())

web.config.debug = False

if __name__ == "__main__": 
	app.run()
