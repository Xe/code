package main

import (
	"database/sql"
	"log"

	"github.com/Xe/hive/common"
	_ "github.com/lib/pq"
)

func main() {
	u, err := common.Connect()
	if err != nil {
		log.Fatal(err)
	}

	db, err := sql.Open("postgres", "user=root host=cockroachdb dbname=hive port=26257 sslmode=disable")
	if err != nil {
		log.Fatal(err)
	}

	_, err = db.Exec("CREATE TABLE IF NOT EXISTS Foo (id INTEGER DEFAULT unique_rowid(), name TEXT);")
	if err != nil {
		log.Fatal(err)
	}

	_, err = db.Exec("INSERT INTO Foo VALUES (DEFAULT, $1);", u.UUID)
	if err != nil {
		log.Fatal(err)
	}
}
