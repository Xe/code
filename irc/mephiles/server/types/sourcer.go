package types

// Sourcer is an interface that defines the source of a message.
type Sourcer interface {
	ID() string
}
