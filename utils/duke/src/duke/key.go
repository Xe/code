package duke

import (
	"encoding/gob"
	"fmt"
	"strings"
)

// Key is an ssh key with an associated user
type Key struct {
	Username    string
	Kind        string
	PublicKey   string
	Comment     string
	Fingerprint string
}

func init() {
	gob.Register(&Key{})
}

// NewKey returns a new Key structure based on a username and the body of an SSH key.
func NewKey(username, pubkey string) *Key {
	pkSplit := strings.Split(pubkey, " ")

	comment := strings.Split(pkSplit[2], "\n")[0]

	return &Key{
		Username:    username,
		Kind:        pkSplit[0],
		PublicKey:   pkSplit[1],
		Comment:     comment,
		Fingerprint: getFingerprint(pkSplit[1]),
	}
}

// String serializes a Key as an openssh authorized_keys format key.
func (k *Key) String() string {
	return fmt.Sprintf("%s %s %s", k.Kind, k.PublicKey, k.Comment)
}
