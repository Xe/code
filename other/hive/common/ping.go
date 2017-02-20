package common

type Pong struct {
	Token string `json:"token"`
	Host  string `json:"host"`
	UUID  string `json:"uuid"`
	Role  string `json:"role"`
}

func HandlePingGenerator(u *Uplink) func(string) {
	return func(foo string) {
		p := &Pong{
			Token: foo,
			Host:  *host,
			UUID:  u.UUID,
			Role:  *role,
		}

		err := u.Publish("hive.mast.event.service.pong", p)
		if err != nil {
			panic(err)
		}
	}
}
