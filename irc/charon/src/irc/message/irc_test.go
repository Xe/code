package message

import (
	"encoding/json"
	"reflect"
	"testing"
)

var testCases = [][]string{
	[]string{
		":Alissa!AlissaSqua@lightning.bouncer.ml PRIVMSG #coders :idfk much about vulnerabilities except for the n2p one doing something with SSL",
		"{\"tags\":{},\"source\":\"Alissa!AlissaSqua@lightning.bouncer.ml\",\"verb\":\"PRIVMSG\",\"args\":[\"#coders\",\"idfk much about vulnerabilities except for the n2p one doing something with SSL\"]}",
	},
	[]string{
		":chrysalis.canternet.org PONG chrysalis.canternet.org :irc.canternet.org",
		"{\"tags\":{},\"source\":\"chrysalis.canternet.org\",\"verb\":\"PONG\",\"args\":[\"chrysalis.canternet.org\",\"irc.canternet.org\"]}",
	},
	[]string{
		"PING irc.canternet.org",
		"{\"tags\":{},\"source\":\"\",\"verb\":\"PING\",\"args\":[\"irc.canternet.org\"]}",
	},
	[]string{
		":duperghoul!foorhgvdbm@j73.088.58.71.dynamic.ip.windstream.net PRIVMSG #CinemaQuestria :pft,that accent!! Better than mine, and I'm in Texas!",
		"{\"tags\":{},\"source\":\"duperghoul!foorhgvdbm@j73.088.58.71.dynamic.ip.windstream.net\",\"verb\":\"PRIVMSG\",\"args\":[\"#CinemaQuestria\",\"pft,that accent!! Better than mine, and I'm in Texas!\"]}",
	},
	[]string{
		":adagiodazzle.ponychat.net NOTICE * :*** Notice -- Client connecting: pony_4982 (pony_4982@ip-removed) [ip-removed] {?} [qwebirc]",
		"{\"tags\":{},\"source\":\"adagiodazzle.ponychat.net\",\"verb\":\"NOTICE\",\"args\":[\"*\",\"*** Notice -- Client connecting: pony_4982 (pony_4982@ip-removed) [ip-removed] {?} [qwebirc]\"]}",
	},
	[]string{
		"@aaa=bbb;ccc;example.com/ddd=eee :nick!ident@host.com PRIVMSG me :Hello",
		"{\"tags\":{\"aaa\":\"bbb\",\"ccc\":\"true\",\"example.com/ddd\":\"eee\"},\"source\":\"nick!ident@host.com\",\"verb\":\"PRIVMSG\",\"args\":[\"me\",\"Hello\"]}",
	},
}

func TestParseMessage(t *testing.T) {
	var test_message Message
	var parsed_message Message

	for _, testcase := range testCases {
		parsed_message = *ParseMessage(testcase[0])
		err := json.Unmarshal([]byte(testcase[1]), &test_message)
		if err != nil {
			t.Fatalf("%s failed to decode", testcase[1])
		}
		if !reflect.DeepEqual(test_message, parsed_message) {
			t.Fatalf("%s failed to output the correct data\nExpected:\t%v\nGot:\t\t%v", testcase[0], test_message, parsed_message)
		}
	}

}
