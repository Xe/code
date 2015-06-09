package commands

import (
	"strings"

	"github.com/Xe/mephiles/server/globals"
	"github.com/Xe/mephiles/server/types"
)

type clientNickCommand struct {
	ArgsNeeded int
}

func (cc clientNickCommand) ClientCommand(c *types.Client, verb string, args []string) (m types.Message) {
	// NICK :Foo
	nick := args[0]
	searchNick := strings.ToUpper(nick)

	if _, present := globals.Clients.ByNick[searchNick]; present {
		return types.Message{
			Verb:    "433",
			Message: "Nickname is already in use",
		}
	}

	c.Nick = nick

	return
}

func (cc clientNickCommand) NumArgs() int {
	return cc.ArgsNeeded
}

func init() {
	globals.ClientCommands["NICK"] = clientNickCommand{1}
}
