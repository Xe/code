package main

// Incoming commands
type InCommand int

//go:generate stringer -type=InCommand

// Commands that ssld recieves
const (
	CommandIncNone InCommand = iota // No command
	CommandIncAccept
	CommandIncConnect
	CommandIncKeys
	CommandIncInitPRNG
	CommandIncStats
	CommandIncChangeConnid
)

// Map of incoming commands -> names
var (
	IncomingCommandsToNames = map[string]InCommand{
		"A": CommandIncAccept,
		"C": CommandIncConnect,
		"K": CommandIncKeys,
		"I": CommandIncInitPRNG,
		"S": CommandIncStats,
		"Y": CommandIncChangeConnid,
	}
)

// Outgoing commands
type OutCommand int

//go:generate stringer -type=OutCommand

// Commands that ssld sends
const (
	CommandOutNone        OutCommand = iota // No command
	CommandOutIAmUseless                    // ssld is useless, wait for death
	CommandOutNoSSL                         // No ssl support, ssld is largely useless
	CommandOutInvalidKeys                   // SSL keys supplied are invalid
	CommandOutStats                         // Stats reply
	CommandOutFingerprint                   // Client by id has a SSL fingerprint
	CommandOutCloseConn                     // ssld is closing a connection
	CommandOutNoZlib                        // libratbox has no zlib support
)

// Map of outgoing names -> commands
var (
	OutgoingNamesToCommands = map[OutCommand]string{
		CommandOutIAmUseless:  "U",
		CommandOutNoSSL:       "N",
		CommandOutInvalidKeys: "I",
		CommandOutStats:       "S",
		CommandOutFingerprint: "F",
		CommandOutCloseConn:   "D",
		CommandOutNoZlib:      "z",
	}
)
