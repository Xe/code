package globals

import (
	"testing"
)

func testValidNick(t *testing.T, nick string, expected bool) {
	if expected {
		if !REValidNickName.MatchString(nick) {
			t.Fatal(nick + " is not a valid nickname")
		}
	} else {
		if REValidNickName.MatchString(nick) {
			t.Fatal(nick + " is and should not be a valid nickname")
		}
	}
}

func TestNickFoo(t *testing.T) {
	testValidNick(t, "foo", true)
}

func TestNickWithNumberAtBeginning(t *testing.T) {
	testValidNick(t, "0foo", false)
}

func TestNickWithUnicode(t *testing.T) {
	testValidNick(t, "ReaperOfDeath(◕‿◕)", false)
}

func TestNickThatIs31Chars(t *testing.T) {
	testValidNick(t, "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", true)
}

func TestNickThatIs32Chars(t *testing.T) {
	testValidNick(t, "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", false)
}
