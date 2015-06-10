+++
title = "config"
+++

```go
import "github.com/Xe/Tetra/bot/config"
```

Package config holds the configuration for Tetra.

## Usage

#### type AthemeConfig

```go
type AthemeConfig struct {
	URL      string
	Username string
	Password string
}
```

Atheme configuration

#### type Config

```go
type Config struct {
	Autoload []string
	Services []*ServiceConfig
	Server   *ServerConfig
	Uplink   *UplinkConfig
	Stats    *StatsConfig
	ApiKeys  map[string]string
	General  *GeneralConfig
	Modules  map[string]interface{}
	Etcd     struct {
		Machines []string
	}
	Atheme *AthemeConfig
}
```

Struct Config defines the configuration for Tetra.

#### func  NewConfig

```go
func NewConfig(fname string) (conf *Config, err error)
```
NewConfig returns a new Config instance seeded by the file at fname.

#### type GeneralConfig

```go
type GeneralConfig struct {
	StaffChan string
	SnoopChan string
	Prefix    string
	Debug     bool
	Workers   int
}
```

Struct GeneralConfig defines general configuration for Tetra.

#### type ServerConfig

```go
type ServerConfig struct {
	Name  string
	Gecos string
	Sid   string
}
```

Struct ServerConfig defines the server information for Tetra.

#### type ServiceConfig

```go
type ServiceConfig struct {
	Nick   string
	User   string
	Host   string
	Gecos  string
	Name   string
	Certfp string
}
```

Struct ServiceConfig defines the configuration for a service.

#### type StatsConfig

```go
type StatsConfig struct {
	Host     string
	Database string
	Username string
	Password string
}
```

Struct StatsConfig defines the InfluxxDB information for Tetra.

#### type UplinkConfig

```go
type UplinkConfig struct {
	Host     string
	Port     string
	Password string
	Ssl      bool
}
```

Struct UplinkConfig defines the configuration of Tetra's uplink.
