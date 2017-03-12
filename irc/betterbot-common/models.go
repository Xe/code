package common

/*
Message represents a protocol-agnostic message.
*/
type Message struct {
	ID       string `json:"id"`
	Protocol string `json:"protocol"`
	ReplyTo  string `json:"replyto"`
	Via      string `json:"via"` // uuid of the service to send this to

	Body              string                 `json:"body"`
	BodyParams        map[string]interface{} `json:"bodyparams"`
	Sender            string                 `json:"sender"`
	SenderParams      map[string]interface{} `json:"senderparams"`
	Destination       string                 `json:"destination"`
	DestinationParams map[string]interface{} `json:"destinationparams"`

	Kind     string                 `json:"kind"`
	Metadata map[string]interface{} `json:"metadata"`
	Incoming bool                   `json:"incoming"`
}

/*
Birth is what a service sends when it first comes alive.
*/
type Birth struct {
	ID         string   `json:"id"` // Also functions as a ping-channel
	EventKinds []string `json:"kinds"`
}
