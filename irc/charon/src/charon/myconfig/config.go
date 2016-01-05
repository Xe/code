package myconfig

import (
	"time"

	"github.com/scalingdata/gcfg"
)

type Config struct {
	Default Default
	Server  Server
	Log     Log
	Oper    map[string]*Oper
	TLS     TLS
	Listen  Listen
	Redis   Redis
}

type Default struct {
	KickReason  string
	QuitReason  string
	PartReason  string
	KillReason  string
	KlineReason string

	Cmode string
}

type Redis struct {
	Host     string
	Port     int
	Password string
}

type Server struct {
	Name          string
	Description   string
	PingTime      time.Duration
	PingCheckTime time.Duration
	StatTime      time.Duration
	Salt          string
	ResolveHosts  bool
	Cloaking      bool
	JoinChannel   []string

	Privacy bool
}

type Log struct {
	Channel []string
	File    string
}

type Oper struct {
	Password  string
	Encrypted bool
	Vhost     string
}

type Listen struct {
	PlainPort []int
	TLSPort   []int
}

type TLS struct {
	CertPath string
	KeyPath  string
}

func Load(fname string) (*Config, error) {
	cfg := &Config{}

	err := gcfg.ReadFileInto(cfg, fname)

	return cfg, err
}
