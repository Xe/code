package common

import (
	"flag"
	"fmt"
	"log"
	"os"
	"time"

	"github.com/Xe/hive/utils"
	"github.com/nats-io/nats"
	"github.com/nats-io/nuid"
)

var (
	parent            = flag.String("uplink.parent", "localhost", "the name of the machine hosting this worker's container")
	role              = flag.String("uplink.role", "worker", "the name of the work queue this will join per group interested in")
	host              = flag.String("uplink.host", "nats://nats:4222", "queue uplink")
	pid               = flag.String("uplink.pid", "", "custom ID for this uplink instance combination")
	useGob            = flag.Bool("uplink.gob", false, "use gob encoding?")
	ephemeralService  = flag.Bool("uplink.ephemeral", false, "make this last only up to duration without work?")
	keepaliveDuration = flag.Duration("uplink.feedtime", 30*time.Second, "how often the watchdog should be fed")
)

type Uplink struct {
	*nats.EncodedConn
	*log.Logger
	Nats *nats.Conn
	UUID string

	Watchdog *utils.Watchdog
}

// Connect kicks off a connection to the Uplink, optionally setting up watchdogs
// and the like.
func Connect() (u *Uplink, err error) {
	u = &Uplink{}
	u.Watchdog = &utils.Watchdog{}
	u.Watchdog.Duration = *keepaliveDuration

	if *ephemeralService {
		u.Watchdog.Feed()
		u.Watchdog.Start()
	}

	nc, err := nats.Connect(*host)
	if err != nil {
		return nil, err
	}

	encoder := nats.JSON_ENCODER
	if *useGob {
		encoder = nats.GOB_ENCODER
	}

	ec, err := nats.NewEncodedConn(nc, encoder)
	if err != nil {
		return nil, err
	}

	u.EncodedConn = ec
	u.Nats = nc

	if *pid == "" {
		u.UUID = nuid.Next()
	} else {
		u.UUID = *pid
	}

	u.Logger = log.New(os.Stdout, fmt.Sprintf("%s %s ", *role, u.UUID), log.Ldate|log.Ltime|log.Lshortfile)

	err = ec.Publish("announce", fmt.Sprintf("Worker %s online.", u.UUID))
	if err != nil {
		return nil, err
	}

	log.Printf("Worker %s (%s) online", u.UUID, os.Args[0])

	u.Subscribe("hive.mast.request.ping", HandlePingGenerator(u))
	u.Subscribe("hive."+u.UUID+".ping", HandlePingGenerator(u))

	return
}
