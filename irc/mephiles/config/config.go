package config

import (
	"time"
)

// ConnType is the type of connection the Listen struct listens for.
type ConnType string

// The following is a list of connection types that are supported in Mehpiles.
const (
	PlainConn     = "PLAIN"     // Plain text connection
	SSLConn       = "SSL"       // SSL connection
	WebsocketConn = "WEBSOCKET" // Websocket connection
)

// ConnFlag is the possible connection flags that can be set via I:Lines.
type ConnFlag string

// The following is a list of all possible connection flags
const (
	DNSBlProof = "dnsbl_exempt" // Makes users matching this connection immune to DNSBL checks
	KlineProof = "kline_exempt" // K:Lines have no affect
	NoTilde    = "no_tilde"     // Do not prefix ~ if ident check fails
	NeedIdent  = "need_ident"   // Reject connection without identd
)

// Config is the master configuration structure for Mephiles.
type Config struct {
	// Local may only have configuration other servers will not share
	Local struct {
		Name        string
		SID         string
		Description string

		SSLPrivateKey string // SSL private key
		SSLCert       string // SSL certificate
	}

	Network struct {
		Name     string // Network name
		Helpchan string // Help channel
		HelpURL  string // Help website

		// RFC 1459 compliance
		Admin struct {
			Name        string
			Description string
			Email       string
		}

		Classes   map[string]*Class
		Authlines map[string]*Authline
		Staff     map[string]*Staff

		Ports []*Listen
	}
}

// Class is a connection class
type Class struct {
	Name         string
	PingTime     time.Duration
	LocalClones  int
	GlobalClones int
	Sendq        int64 // in bytes
}

// Listen defines ports to listen on
type Listen struct {
	Host string
	Port string
	Kind ConnType
}

// Authline allows a user to connect to the IRC daemon, old I:Line.
type Authline struct {
	Mask     []string   // mask{,s} that match this AuthLine
	Password string     // Password if applicable
	WebIRC   bool       // Is this a web IRC connection?
	Class    string     // Connection class to lop them under
	Flags    []ConnFlag // Connection flags to apply
}

// Staff defines a network staff member. Delegation of privileges is not supported.
// If a user is not trusted enough for all privileges, do not give them access.
type Staff struct {
	Name          string   // Name to /oper up with
	User          []string // user@host masks to allow opering up from
	Password      string   // Password for opering up
	Fingerprint   string   // Optional SSL fingerprint required to oper up
	Snomask       string   // Server notice mask to apply
	VHost         string   // Vhost to apply to the oper
	SWhois        string   // Second whois line
	Operstring    string   // "Is a Swag Master"
	CryptPassword bool     // Is password encrypted?
}
