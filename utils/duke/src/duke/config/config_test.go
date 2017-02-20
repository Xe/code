package config

import (
	"testing"

	"github.com/scalingdata/gcfg"
)

func TestParseConfig(t *testing.T) {
	c := &Config{}

	err := gcfg.ReadFileInto(c, "../../../etc/duke.ini")
	if err != nil {
		t.Fatal(err)
	}
}
