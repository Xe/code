package main

import (
	"os"
	"testing"
)

func TestWorldIsSane(t *testing.T) {
	os.Setenv("KREWELLA_NETWORKS", "PonyChat")
	os.Setenv("PONYCHAT_NICK", "Jeffrey")
	os.Setenv("PONYCHAT_USER", "krewella")
	os.Setenv("PONYCHAT_HOST", "127.0.0.1")
	os.Setenv("PONYCHAT_PORT", "6067")

	err := createBots()
	if err != nil {
		t.Fatal(err)
	}
}
