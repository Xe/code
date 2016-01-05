package cqbot

import (
	"os"

	"code.google.com/p/gcfg"
)

type ServerConfig struct {
	Port string
	Host string
}

type BotConfig struct {
	Nick    string
	User    string
	Gecos   string
	Channel string
	Nspass  string
	Prefix  string
}

type Config struct {
	Server ServerConfig
	Bot    BotConfig
}

// Wrap the ini file and load the configuration
func LoadConfig(cfgFile string) (cfg Config) {
	err := gcfg.ReadFileInto(&cfg, cfgFile)

	if err != nil {
		panic(err)
	}

	if cfg.Server.Host == "" {
		if val := os.Getenv("IRCD_PORT_6667_TCP_ADDR"); val != "" {
			cfg.Server.Host = val
		} else {
			panic("No host")
		}
	}

	return
}

func writeLine(fout *os.File, line string) {
	num, err := fout.Write([]byte(line + "\n"))
	if num != len(line) {
		return
	}
	if err != nil {
		panic(err)
	}
}

func (conf *Config) Export(fname string) (err error) {
	fout, err := os.Create(fname)
	if err != nil {
		panic(err)
	}
	defer fout.Close()

	writeLine(fout, "[server]")
	writeLine(fout, "port = "+conf.Server.Port)
	writeLine(fout, "host = "+conf.Server.Host)
	writeLine(fout, "\n[bot]")
	writeLine(fout, "nick = "+conf.Bot.Nick)
	writeLine(fout, "user = "+conf.Bot.User)
	writeLine(fout, "gecos = "+conf.Bot.Gecos)
	writeLine(fout, "channel = "+conf.Bot.Channel)
	writeLine(fout, "nspass = "+conf.Bot.Nspass)
	writeLine(fout, "prefix = "+conf.Bot.Prefix)

	return
}
