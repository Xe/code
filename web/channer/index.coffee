express = require "express"
getenv = require "getenv"
mysql = require "mysql"

connection = mysql.createConnection {
  host: getenv "MYSQL_HOST", getenv("MYSQL_PORT_3306_TCP_ADDR", "localhost")
  user: getenv "MYSQL_USER", "root"
  password: getenv "MYSQL_PASSWORD", getenv("MYSQL_ENV_MYSQL_ROOT_PASSWORD", "flapnars")
}

app = do express

app.set "views", "./views"
app.set "view engine", "jade"

app.engine "jade", require("jade").__express

app.use require("morgan")("combined")
app.use express.static __dirname+"/public"

app.get "/", (req, res) ->
  res.render "index", {
    title: "Hey"
    message: "Hello world"
  }

server = app.listen getenv.int("PORT", 3000), ->
  console.log "Listening on port " + server.address().port
