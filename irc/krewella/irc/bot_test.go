package irc

import (
	"os"
	"testing"
	"time"
)

func TestFailToMakeBot(t *testing.T) {
	_, err := New("PonyChat")

	if err != ErrBadConfig {
		t.Fatalf("Expected bad configuration error, got %#v", err)
	}
}

func TestMakeBot(t *testing.T) {
	os.Setenv("PONYCHAT_NICK", "Jeffrey")
	os.Setenv("PONYCHAT_USER", "krewella")
	os.Setenv("PONYCHAT_HOST", "127.0.0.1")
	os.Setenv("PONYCHAT_PORT", "6067")

	bot, err := New("PonyChat")

	if err == ErrBadConfig {
		t.Fatal("Configuration somehow bad")
	}

	if err != nil {
		t.Fatal(err)
	}

	time.Sleep(500 * time.Millisecond)
	bot.IrcObj.Quit()
}

func TestJoinActuallyHasToJoin(t *testing.T) {
	bot, err := New("PonyChat")

	if err != nil {
		t.Fatal(err)
	}

	if !bot.Join("#foo") {
		t.Fatal("Couldn't join #foo")
	}

	time.Sleep(500 * time.Millisecond)
	bot.IrcObj.Quit()
}

func TestJoinDoesntDoubleJoin(t *testing.T) {
	bot, err := New("PonyChat")

	if err != nil {
		t.Fatal(err)
	}

	bot.Join("#foo")

	if bot.Join("#foo") {
		t.Fatal("needed to join #foo again? wtf.")
	}

	time.Sleep(500 * time.Millisecond)
	bot.IrcObj.Quit()
}
