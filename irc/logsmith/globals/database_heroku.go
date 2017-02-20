// +build heroku

package globals

import (
	"os"
	"strings"

	"github.com/jinzhu/gorm"
	_ "github.com/lib/pq"
)

var (
	DB gorm.DB
)

func init() {
	var err error
	DB, err = gorm.Open("postgres", strings.Replace(os.Getenv("DATABASE_URL"), "postgresql", "postgres", 1))
	if err != nil {
		panic(err)
	}
}
