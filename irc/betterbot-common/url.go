package common

import (
	"io"
	"log"
	"os"

	"github.com/Xe/uuid"
	"github.com/namsral/flag"
	"github.com/nats-io/nats"
)

var (
	natsServer = flag.String("nats-url", "nats://nats:4242", "where to connect to for nats")
)

// Uplink to the hive
type Uplink struct {
	*nats.EncodedConn
	*log.Logger

	Nats *nats.Conn
	ID   string
}

// Connect just connects you to nats, no fuss.
func Connect() (*Uplink, error) {
	nc, err := nats.Connect(*natsServer)
	if err != nil {
		return nil, err
	}

	c, err := nats.NewEncodedConn(nc, nats.JSON_ENCODER)
	if err != nil {
		return nil, err
	}

	id := uuid.New()

	u := &Uplink{
		EncodedConn: c,
		Logger:      log.New(os.Stdout, id+" ", log.LstdFlags),
		ID:          id,
		Nats:        nc,
	}

	_, err = c.Subscribe(u.ID+":ping", func(subj, reply string, msg string) {
		c.Publish(reply, "pong")
	})
	if err != nil {
		return nil, err
	}

	return u, nil
}

func autoclose(c io.Closer) {
	if err := c.Close(); err != nil {
		log.Printf("%v", err)
	}
}
