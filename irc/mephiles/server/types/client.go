package types

import (
	"io"
	"net"

	"github.com/Xe/mephiles/server/modes"
)

// ConnFlag is a connection flag such as "kline exempt" or "no ident lookup"
type ConnFlag int64

// Possible connection flags
const (
	ConnFlagNone   = iota
	ConnFlagNoNick // Connection is waiting on the NICK command to be sent
	ConnFlagNoUser // Connection is waiting on the USER command to be sent
	ConnFlagNokline
	ConnFlagNoident
	ConnFlagNodnsbl
	ConnFlagFloodexempt
)

// Client is a client on the IRC network
type Client struct {
	Conn   io.ReadWriteCloser // if this is nil it is a remote client
	Server *Server            // The server they are connected to

	Nick     string // Nickname
	User     string // user name
	Vhost    string // Visible host, can be what cloaking spits out
	ConnHost string // Real host for the connection, looked up with reverse DNS
	TS6ID    string // Server+host ID
	UUID     string // Type 1 UUID
	Account  string // Account the user is logged in as

	inq          int64 // Amount of data the client can send
	outq         int64 // Amount of data the client can receive
	LastPongTime int64 // time the client last replied to PING

	Metadata   map[string]string // Client metadata
	Properties modes.UserFlag    // Client user modes
}

// IsLocal returns true if the client is a local client or false if it is a
// remote client
func (c *Client) IsLocal() bool {
	return c.Conn != nil
}

// NewClient makes a new client out of a net.Conn.
func NewClient(conn net.Conn) (c *Client) {
	c = &Client{
		Conn: conn,
	}

	return
}
