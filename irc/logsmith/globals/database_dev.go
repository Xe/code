// +build !heroku

package globals

import (
	"github.com/jinzhu/gorm"
	_ "github.com/mattn/go-sqlite3"
)

var (
	DB gorm.DB
)

func init() {
	var err error
	DB, err = gorm.Open("sqlite3", "var/data.db")
	if err != nil {
		panic(err)
	}

	DB.LogMode(true)
}
