+++
title = "script"
+++

```go
import "github.com/Xe/Tetra/bot/script"
```

Package script is for the resuable lua scripting engine that Tetra uses.

## Usage

```go
const (
	INV_COMMAND = 0x0001
	INV_NAMHOOK = 0x0002
	INV_PROHOOK = 0x0004
)
```
The different kinds of invocations that can be called upon.

#### type Invocation

```go
type Invocation struct {
	Kind     int
	Args     []interface{}
	Reply    chan string
	Function *luar.LuaObject
	Line     *r1459.RawLine
}
```

Struct Invocation represents an event from Go->Lua.

#### type Script

```go
type Script struct {
	Name    string
	L       *lua.State
	Log     *log.Logger
	Service string
	Uuid    string
	Kind    string
}
```

Struct Script implements a Lua scripting interface to Tetra.

#### func  NewScript

```go
func NewScript(name string) (script *Script, err error)
```
LoadScript finds and loads the appropriate script by a given short name
(tetra/die).

#### func (*Script) Unload

```go
func (s *Script) Unload(name string) error
```
Unload a script and delete its commands and handlers
