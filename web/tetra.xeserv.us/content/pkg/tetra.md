+++
title = "tetra"
+++

```go
import "github.com/Xe/Tetra/bot"
```

Package tetra implements the core for a TS6 pseudoserver. It also has lua and
moonscript loading support to add functionality at runtime.

## Usage

```go
const (
	INV_COMMAND = 0x0001
	INV_NAMHOOK = 0x0002
	INV_PROHOOK = 0x0004
)
```
The different kinds of invocations that can be called upon.

```go
var (
	Conn     *Connection
	Info     *Server
	Clients  *ClientSet
	Channels map[string]*Channel
	Bursted  bool
	Handlers map[string]map[string]*Handler
	Services map[string]*Client
	Servers  map[string]*Server
	Scripts  map[string]*Script
	Hooks    map[string][]*Hook

	ActiveConfig *config.Config
	Log          *log.Logger
	Uplink       *Server

	Etcd   *etcd.Client
	Atheme *atheme.Atheme
	Cron   *cron.Cron
)
```
Struct Tetra contains all fields for

```go
var (
	Libraries map[string]luar.Map
)
```

#### func  Auth

```go
func Auth()
```
Auth authenticates over TS6.

#### func  Burst

```go
func Burst()
```
Burst sends our local information after recieving the server's burst.

#### func  Connect

```go
func Connect(host, port string) (err error)
```
Connect connects to the uplink server.

#### func  DelHandler

```go
func DelHandler(verb string, uuid string) (err error)
```
DelHandler deletes a handler for a given protocol verb by the UUID of the
handler.

#### func  DelHook

```go
func DelHook(hook *Hook) (err error)
```
DelHook deletes a hook. Returns an error if there is no such hook to delete.

#### func  DelService

```go
func DelService(service string) (err error)
```
DelService deletes a service from the network or returns an error.

#### func  GetChannelStats

```go
func GetChannelStats(...interface{})
```

#### func  GetNetworkStats

```go
func GetNetworkStats(...interface{})
```

#### func  GetYo

```go
func GetYo(name string) (client *yo.Client, err error)
```
GetYo returns an instance of yo.Client based on the username being present in
the apikeys section of the configuration file.

#### func  Main

```go
func Main()
```
Main is the main loop.

#### func  NewTetra

```go
func NewTetra(cpath string)
```
NewTetra returns a new instance of Tetra based on a config file located at
cpath. This also kicks off the worker goroutines and statistics collection, as
well as seeding basic protocol verb handlers.

#### func  NextUID

```go
func NextUID() string
```
NextUID returns a new TS6 UID.

#### func  ProcessLine

```go
func ProcessLine(line string)
```
ProcessLine processes a line as if it came from the server.

#### func  Quit

```go
func Quit()
```
Quit kills Tetra gracefully.

#### func  RunHook

```go
func RunHook(verb string, args ...interface{}) (err error)
```
RunHook runs a hook in parallel across multiple goroutines, one per
implementaion of the hook. Returns error if there is no such hook.

#### func  StickConfig

```go
func StickConfig()
```
StickConfig creates Clients based off of the config file and handles module
autoloads.

#### func  UnloadScript

```go
func UnloadScript(name string) error
```
Unload a script and delete its commands and handlers

#### func  WebApp

```go
func WebApp()
```
WebApp creates the web application and YAML api for Tetra.

#### type ChanUser

```go
type ChanUser struct {
	Client  *Client
	Channel *Channel
	Prefix  int
}
```

Struct ChanUser is a wrapper around a Channel and a Client to represent
membership in a Channel.

#### type Channel

```go
type Channel struct {
	Name     string
	Ts       int64
	Modes    int
	Clients  map[string]*ChanUser
	Lists    map[int][]string
	Gauge    metrics.Gauge
	Metadata map[string]string
}
```

Struct Channel holds all the relevant data for an IRC channel. A lot of this is
not just things defined in RFC 1459, but extensions like the TS. This implements
Targeter

#### func  NewChannel

```go
func NewChannel(name string, ts int64) (c *Channel)
```
NewChannel creates a new channel with a given name and ts.

#### func (*Channel) AddChanUser

```go
func (c *Channel) AddChanUser(client *Client) (cu *ChanUser)
```
AddChanUser adds a client to the channel, returning the membership.

#### func (*Channel) DelChanUser

```go
func (c *Channel) DelChanUser(client *Client) (err error)
```
DelChanUser deletes a client from a channel or returns an error.

#### func (*Channel) IsChannel

```go
func (c *Channel) IsChannel() bool
```
IsChannel returns true.

#### func (*Channel) Target

```go
func (c *Channel) Target() string
```
Target returns a targetable version of Channel.

#### type Client

```go
type Client struct {
	Nick        string
	User        string
	Host        string
	VHost       string
	Ip          string
	Account     string
	Uid         string
	Gecos       string
	Permissions int
	Umodes      int
	Kind        string
	Ts          int64
	Channels    map[string]*Channel
	Server      *Server
	Commands    map[string]*Command
	Certfp      string
	Metadata    map[string]string
}
```

Struct Client holds information about a client on the IRC network.

#### func  AddService

```go
func AddService(service, nick, user, host, gecos, certfp string) (cli *Client)
```
AddService adds a new service Client to the network.

#### func (*Client) Chghost

```go
func (r *Client) Chghost(target *Client, newhost string) (err error)
```
Chghost changes a client's visible host

#### func (*Client) Euid

```go
func (r *Client) Euid() string
```
Euid returns an EUID burst.

#### func (*Client) IsChannel

```go
func (r *Client) IsChannel() bool
```
IsChannel returns false.

#### func (*Client) IsOper

```go
func (r *Client) IsOper() bool
```
IsOper returns if the client is an operator or not.

#### func (*Client) Join

```go
func (r *Client) Join(channame string)
```
Join makes the client join a channel. This does not check bans.

#### func (*Client) Kill

```go
func (r *Client) Kill(target *Client, reason string)
```
Kill kills a target client

#### func (*Client) NewCommand

```go
func (c *Client) NewCommand(verb string, handler func(*Client, Targeter, []string) string) (cmd *Command, err error)
```
NewCommand returns a new command instance.

#### func (*Client) Notice

```go
func (r *Client) Notice(destination Targeter, message string)
```
Notice sends a NOTICE to destination with given message.

#### func (*Client) OperLog

```go
func (r *Client) OperLog(message string)
```
OperLog logs a given message to the operator channel.

#### func (*Client) Part

```go
func (r *Client) Part(channame string) bool
```
Part makes the client leave a channel.

#### func (*Client) Privmsg

```go
func (r *Client) Privmsg(destination Targeter, message string)
```
Privmsg sends a PRIVMSG to destination with given message.

#### func (*Client) Quit

```go
func (r *Client) Quit()
```
Quit quits a client off of the network.

#### func (*Client) ServicesLog

```go
func (r *Client) ServicesLog(message string)
```
ServicesLog logs a given message to the services snoop channel.

#### func (*Client) Target

```go
func (r *Client) Target() string
```
Target returns a targetable version of a Client.

#### type ClientSet

```go
type ClientSet struct {
	ByNick map[string]*Client
	ByUID  map[string]*Client
	Gauge  metrics.Gauge
}
```

Struct Clients defines the set of clients on the network, indexed by either
nickname (in capital letters) or UID.

#### func (*ClientSet) AddClient

```go
func (c *ClientSet) AddClient(client *Client)
```
AddClient adds a Client to the Clients structure.

#### func (*ClientSet) ChangeNick

```go
func (c *ClientSet) ChangeNick(client *Client, newnick string) (err error)
```
ChangeNick changes a client's nickname and updates the ByNick map.

#### func (*ClientSet) DelClient

```go
func (c *ClientSet) DelClient(client *Client) (err error)
```
DelClient deletes a Client from the Clients structure.

#### type Command

```go
type Command struct {
	Impl      func(*Client, Targeter, []string) string
	Uuid      string
	Script    *Script
	Verb      string
	Owner     *Client
	NeedsOper bool
}
```

Struct command holds everything needed for a bot command.

#### type Connection

```go
type Connection struct {
	Conn   net.Conn
	Log    *log.Logger
	Reader *bufio.Reader
	Tp     *textproto.Reader
	Buffer chan string

	Debug bool
}
```

Struct Connection contains everything needed for the socket connection Tetra
uses.

#### func (*Connection) Close

```go
func (c *Connection) Close()
```
Close kills the connection

#### func (*Connection) GetLine

```go
func (c *Connection) GetLine() (line string, err error)
```
GetLine returns a new line from the server.

#### func (*Connection) SendLine

```go
func (c *Connection) SendLine(line string, stuff ...interface{})
```
SendLine buffers a line to be sent to the server.

#### type Handler

```go
type Handler struct {
	Impl   func(*r1459.RawLine)
	Verb   string
	Uuid   string
	Script *Script
	Go     bool
}
```

Struct Handler defines a raw protocol verb handler. Please do not use this
unless you have good reason to.

#### func  AddHandler

```go
func AddHandler(verb string, impl func(*r1459.RawLine)) (handler *Handler, err error)
```
AddHandler adds a handler for a given verb.

#### type Hook

```go
type Hook struct {
	Uuid string

	Owner *Script
	Verb  string
}
```

Struct Hook defines a command hook for Tetra. This can be used for hooking on
events (like being yo'd).

#### func  NewHook

```go
func NewHook(verb string, impl func(...interface{})) (h *Hook)
```
NewHook allocates and returns a new Hook structure

#### type Invocation

```go
type Invocation struct {
	Kind     int
	Args     []interface{}
	Reply    chan string
	Function *luar.LuaObject
	Client   *Client
	Target   Targeter
	Line     *r1459.RawLine
}
```

Struct Invocation represents an event from Go->Lua.

#### type Script

```go
type Script struct {
	Name     string
	L        *lua.State
	Log      *log.Logger
	Handlers map[string]*Handler
	Commands map[string]*Command
	Hooks    []*Hook
	Service  string
	Client   *Client
	Uuid     string
	Kind     string
	Trigger  chan []interface{}
}
```

Struct Script implements a Lua scripting interface to Tetra.

#### func  LoadScript

```go
func LoadScript(name string) (script *Script, err error)
```
LoadScript finds and loads the appropriate script by a given short name
(tetra/die).

#### func (*Script) AddLuaCommand

```go
func (script *Script) AddLuaCommand(verb string, name string) error
```
AddLuaCommand adds a new command to a script from a lua context.

#### func (*Script) AddLuaHook

```go
func (script *Script) AddLuaHook(verb string, name string) error
```
AddLuaHook adds a named hook from lua.

#### func (*Script) AddLuaProtohook

```go
func (script *Script) AddLuaProtohook(verb string, name string) error
```
AddLuaProtohook adds a lua function as a protocol hook.

#### func (*Script) Call

```go
func (s *Script) Call(command string, source *Client, dest Targeter, args []string) (string, error)
```
Call calls a command in a Script.

#### type Server

```go
type Server struct {
	Sid   string
	Name  string
	Gecos string
	Links []*Server

	Counter metrics.Gauge
	Hops    int
	Capab   []string
}
```

Struct Server holds information for a TS6 server.

#### func (*Server) AddClient

```go
func (s *Server) AddClient()
```
AddClient increments the server client counter.

#### func (*Server) DelClient

```go
func (s *Server) DelClient()
```
DelClient decrements the server client counter.

#### type Targeter

```go
type Targeter interface {
	Target() string  // Targetable version of name
	IsChannel() bool // Is this a channel?
}
```

Interface Targeter wraps around Client and Channel to make messaging to them
seamless.
