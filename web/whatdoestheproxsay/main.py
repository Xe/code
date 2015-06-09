import json
import prox
from flask import Flask
app = Flask(__name__)

@app.route("/check/<ip>/<int:port>")
def check(ip, port):
    return json.dumps(prox.check_proxy(ip, port))

if __name__ == '__main__':
    app.run(host="0.0.0.0", debug=True)

