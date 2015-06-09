package types

// Server is a server on the IRC network.
type Server struct {
	Name  string
	ID    string
	UUID  string
	Gecos string

	Links []*Server

	Hops  int
	Capab []string
}
