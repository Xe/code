package cqbot

import (
	"errors"
	"fmt"
	"log"
	"os"
	"strings"

	lua "github.com/aarzilli/golua/lua"
	"github.com/stevedonovan/luar"
)

type Script struct {
	State    *lua.State
	Name     string
	Commands []*Command
	Handlers []*Handler
	Log      *log.Logger
}

// Pre-seed a lua state with things that it would need
func RegisterStuff(bot *Bot, script *Script) {
	luar.Register(script.State, "", luar.Map{
		"script":  script,
		"bot":     bot,
		"sprintf": fmt.Sprintf,
		"printf":  script.Log.Printf,
		"print":   script.Log.Print,
		"NewUser": NewUser,
	})

	script.State.DoFile("bot/base.lua")
}

// Add a lua command (by name) from a lua script. This is designed to be ran
// from a lua environment.
func (script *Script) AddLuaCommand(bot *Bot, verb string, help string, funcname string) {
	function := luar.NewLuaObjectFromName(script.State, funcname)

	command, _ := bot.AddCommand(verb, help, 0, 0,
		func(user *User, message []string) string {
			reply, err := function.Call(user, message)
			if err != nil {
				bot.Log.Printf("Lua error %s: %#v", script.Name, err)
				return fmt.Sprintf("%v", err)
			}

			return reply.(string)
		})

	script.Commands = append(script.Commands, command)
}

// Add a lua function as a protocol hook
func (script *Script) AddLuaProtohook(bot *Bot, verb string, name string) {
	function := luar.NewLuaObjectFromName(script.State, name)

	handler, err := bot.AddHandler(verb, func(line *Line) {
		_, err := function.Call(line)
		if err != nil {
			bot.Log.Printf("Lua error %s: %#v", script.Name, err)
		}
	})
	if err != nil {
		panic(err)
	}

	handler.Script = script
	script.Handlers = append(script.Handlers, handler)
}

// Load a script into memory and register its commands
func (bot *Bot) LoadScript(fname string) (err error) {
	script := &Script{
		State: luar.Init(),
		Name:  fname,
		Log:   log.New(os.Stdout, "script "+fname+" ", log.LstdFlags),
	}

	RegisterStuff(bot, script)

	if strings.HasSuffix(fname, ".lua") {
		res := script.State.DoFile("scripts/" + fname)
		if res != nil {
			panic(res)
		}
	} else if strings.HasSuffix(fname, ".moon") {
		res := script.State.DoString("moon_dofile('scripts/" + fname + "')")
		if res != nil {
			bot.Log.Printf("Moonscript %s didn't load! %#v", fname, res)
		}
	}

	bot.Scripts[fname] = script

	return nil
}

// Unload a script and delete its commands and handlers
func (bot *Bot) UnloadScript(name string) error {
	if _, ok := bot.Scripts[name]; !ok {
		panic(errors.New("No such script"))
	}

	script := bot.Scripts[name]

	for i, command := range script.Commands {
		bot.DelCommand(command.Verb)
		script.Commands = script.Commands[i:]
	}

	for j, handler := range script.Handlers {
		bot.DelHandler(handler.verb, handler.Uid)
		script.Handlers = script.Handlers[j:]
	}

	script.State.Close()

	delete(bot.Scripts, name)

	return nil
}
