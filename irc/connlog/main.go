package main

import (
	"database/sql"
	"fmt"
	"os"
	"time"

	_ "github.com/mattn/go-sqlite3"
	"gopkg.in/alecthomas/kingpin.v1"
)

var (
	app      = kingpin.New("connlog", "Server connection log parser")
	location = app.Flag("weeloc", "Weechat folder to use").Default(os.Getenv("HOME") + "/.weechat").String()

	grepCommand      = app.Command("grep", "searches logs")
	grepKind         = grepCommand.Arg("field", "kind of thing to grep for").Default("ip").String()
	grepContents     = grepCommand.Arg("contents", "what to grep for").String()
	grepNotShowExits = grepCommand.Flag("noexit", "don't show exiting lines").Bool()
	grepTimeFormat   = grepCommand.Flag("timeformat", "time format string. see http://godoc.org/time#pkg-constants").Default(time.ANSIC).String()
	grepfor          map[string]*sql.Stmt

	kindsCommand = app.Command("kinds", "kinds of searches to make")
)

func prepare(stmt string, db *sql.DB) *sql.Stmt {
	s, err := db.Prepare(stmt)
	if err != nil {
		panic(err)
	}

	return s
}

func main() {
	command := kingpin.MustParse(app.Parse(os.Args[1:]))

	_, err := os.Open(*location + "/snotelog.db")
	if err != nil {
		fmt.Fprintf(os.Stderr, "No database at %s\n", *location+"/snotelog.db")
		os.Exit(1)
	}

	db, err := sql.Open("sqlite3", *location+"/snotelog.db")
	if err != nil {
		fmt.Fprintf(os.Stderr, "error opening database: %s", err.Error())
		os.Exit(1)
	}

	if db.Ping() != nil {
		panic("cannot access database")
	}

	grepfor = map[string]*sql.Stmt{
		"ip":     prepare("select * from Connections where ip GLOB ?", db),
		"date":   prepare("select * from Connections where date glob ?", db),
		"server": prepare("select * from Connections where nick glob ?", db),
		"ident":  prepare("select * from Connections where ident glob ?", db),
		"rdns":   prepare("select * from Connections where rdns glob ?", db),
		"gecos":  prepare("select * from Connections where gecos glob ?", db),
	}

	switch command {
	case grepCommand.FullCommand():
		stmt, ok := grepfor[*grepKind]
		if !ok {
			fmt.Fprintf(os.Stderr, "can't search by %s\n", *grepKind)
			os.Exit(1)
		}

		rows, err := stmt.Query(*grepContents)
		if err != nil {
			panic(err)
		}

		defer rows.Close()
		for rows.Next() {
			var id, date, exiting int
			var server, nick, ident, ip, rdns, gecos string

			err := rows.Scan(&id, &date, &exiting, &server, &nick, &ident, &ip, &rdns, &gecos)

			if err != nil {
				panic(err)
			}

			connstring := "connected"
			if exiting == 1 {
				connstring = "exited"
			}

			t := time.Unix(int64(date), 0)

			if exiting == 1 && *grepNotShowExits {
			} else {
				fmt.Printf(
					"%s (%s) %s (%s@%s %s) gecos: %s %s\n",
					t.Format(*grepTimeFormat),
					server,
					nick,
					ident,
					rdns,
					ip,
					gecos,
					connstring,
				)
			}
		}

	case kindsCommand.FullCommand():
		fmt.Println("search kinds registered:")
		for kind, _ := range grepfor {
			fmt.Printf("  - %s\n", kind)
		}

	default:
		app.Usage(os.Stderr)
		os.Exit(1)
	}
}
