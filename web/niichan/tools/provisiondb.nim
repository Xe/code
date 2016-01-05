import db_postgres, os

var
  dbhost = getEnv "POSTGRES_PORT_5432_TCP_ADDR"
  dbport = getEnv "POSTGRES_PORT_5432_TCP_PORT"
  dbpass = getEnv "POSTGRES_ENV_POSTGRES_PASSWORD"

const
  dbuser = "postgres"
  usertomake = "niichan"
  dbtomake = "niichandata"
  pwtomake = "foobang"

var
  db = db_postgres.open("", dbuser, dbpass, "host=" & dbhost & " port=" & dbport)

  schema = [
    sql("CREATE USER " & usertomake & " WITH PASSWORD '" & pwtomake & "';"),
    sql("CREATE DATABASE " & dbtomake & ";"),
    sql("GRANT ALL PRIVILEGES ON DATABASE " & dbtomake & " TO " & usertomake & ";"),
  ]

  tables = [
    sql"""
    CREATE TABLE IF NOT EXISTS Posts(
      id INTEGER PRIMARY KEY NOT NULL,
      uuid VARCHAR(36) NOT NULL,
      board_id INTEGER NOT NULL,
      parent_post INTEGER NOT NULL,
      ip_address VARCHAR(150) NOT NULL,
      body TEXT NOT NULL,
      created_at TIMESTAMP NOT NULL
    );""",
    sql"""
    CREATE TABLE IF NOT EXISTS Boards(
      id INTEGER PRIMARY KEY NOT NULL,
      name VARCHAR(10) NOT NULL,
      description VARCHAR(512) NOT NULL,
      created_at TIMESTAMP NOT NULL
    );""",
  ]

echo "provisioning the database..."

for stanza in schema:
  db.exec stanza

db = db_postgres.open("", dbuser, dbpass, "host=" & dbhost & " port=" & dbport & " dbname=" & dbtomake)

for stanza in tables:
  db.exec stanza
