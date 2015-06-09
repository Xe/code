package types

// Message is an error-like reply with an easy to code and interpret schema.
type Message struct {
	Verb    string
	Message string
}

// Error makes this compatible with the error interface.
func (m *Message) Error() string {
	return m.Verb + ": " + m.Message
}
