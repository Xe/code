import asyncdispatch, db_postgres, jester, os, redissessions

include "./templates/main.tmpl"
include "./templates/meta.tmpl"

var
  dbhost = getEnv "DATABASE_HOST"
  dbport = getEnv "DATABASE_PORT"
  dbuser = getEnv "DATABASE_USER"
  dbpass = getEnv "DATABASE_PASSWORD"
  dbname = getEnv "DATABASE_NAME"

echo "Connecting to ", dbhost, " user ", dbuser, " password ", dbpass, " database ", dbname

var
  db = db_postgres.open("", dbuser, dbpass, "host=" & dbhost & " port=" & dbport & " dbname=" & dbname)

const
  robotstxt = staticRead "robots.txt"

settings:
  port = 8080.Port
  bindAddr = "0.0.0.0"

routes:
  get "/robots.txt":
    resp robotstxt, "text/plain"

  get "/":
    resp "Hello all!".baseTemplate

  get "/meta/rules":
    resp metaRules().baseTemplate "Rules"

  get "/meta/chat":
    resp metaChat().baseTemplate "Chat"

runForever()
