package types

// ArgsNeededer is an interface for something that needs an argument count.
type ArgsNeededer interface {
	NumArgs() int
}

// PreConnectCommander is an interface for a structure that defines an IRC
// daemon command that is processed before the client connection is fully
// completed.
type PreConnectCommander interface {
	ArgsNeededer

	PreConnectCommand(client *Client, verb string, args []string) Message
}

// ClientCommander is an interface for a structure that defines an IRC daemon
// command that clients may call.
type ClientCommander interface {
	ArgsNeededer

	ClientCommand(client *Client, verb string, args []string) Message
}

// S2SCommander is an interface for a structure that defines an IRC daemon
// command that can be called from a server-to-server basis.
type S2SCommander interface {
	ArgsNeededer

	S2SCommand(source *Sourcer, verb string, args []string) Message
}
