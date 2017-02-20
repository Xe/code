package config

// Config is the main configuration wrapper.
type Config struct {
	Server Server
	Client Client
}

// Server is the configuration that a server will use.
type Server struct {
	DataLocation string   // Location of the BoltDB file
	Format       string   // Format that things should serialize in
	User         []string // List of users that this should give keys for, if empty no restrictions
	Listen       string   // What to listen on for HTTP
}

// Client is the configuration for the client.
type Client struct {
	URL string // HTTP URL to connect to
}
