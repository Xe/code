config = require "lapis.config"

config "development", ->
  port 8080

  postgres ->
    backend "pgmoon"
    host "127.0.0.1"
    user "lapis"
    password "lapis"
    database "panel"

config "docker", ->
  port 5000

  postgres ->
    backend "pgmoon"
    port tostring os.getenv "POSTGRESQL_PORT_5432_TCP_PORT"
    host tostring os.getenv "POSTGRESQL_PORT_5432_TCP_ADDR"
    user tostring os.getenv "DB_USER"
    password tostring os.getenv "DB_PASS"
    database tostring os.getenv "DB_NAME"
