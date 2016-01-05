package data

import "time"

/*
A Server is another client or backend node in the network.
*/
type Server struct {
	UUID        string
	Name        string
	Description string
	Kind        ServerKind
	Epoch       time.Time
}

/*
A ServerKind is the role that the server plays in the network.
*/
type ServerKind int

const (
	ServerKindNone     ServerKind = iota // A server that has no kind
	ServerKindLeaf                       // A leaf server that clients connect to
	ServerKindServices                   // A services server
	ServerKindBackend                    // An otherwise unspecified backend server
)
