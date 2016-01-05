package script

import (
	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"runtime"
	"strings"

	"code.google.com/p/go-uuid/uuid"
	"github.com/Xe/Tetra/bot/script/crypto"
	lua "github.com/aarzilli/golua/lua"
	"github.com/stevedonovan/luar"
)

var modules map[string]luar.Map

func init() {
	modules = map[string]luar.Map{
		"uuid": luar.Map{
			"new": uuid.New,
		},
		"web": luar.Map{
			"get":  http.Get,
			"post": http.Post,
		},
		"ioutil": luar.Map{
			"readall":     ioutil.ReadAll,
			"byte2string": byteSliceToString,
		},
		"crypto": luar.Map{
			"hash": crypto.Hash,
			"fnv":  crypto.Fnv,
		},
		"strings": luar.Map{
			"join":  strings.Join,
			"split": strings.Split,
			"first": func(str string) string {
				if len(str) > 0 {
					return string(str[0])
				} else {
					return ""
				}
			},
			"rest": func(str string) string {
				if len(str) > 0 {
					return str[1:]
				} else {
					return ""
				}
			},
			"format": func(format string, args ...interface{}) string {
				return fmt.Sprintf(format, args...)
			},
			"scan": fmt.Sscanf,
			"shuck": func(victim string) string {
				return victim[1 : len(victim)-1]
			},
			"hassuffix": func(s, pattern string) bool {
				return strings.HasSuffix(s, pattern)
			},
		},
	}
}

// Struct Script implements a Lua scripting interface to Tetra.
type Script struct {
	Name string
	L    *lua.State
	Log  *log.Logger
	Uuid string
	Kind string
}

// LoadScript finds and loads the appropriate script by a given short name (tetra/die).
func NewScript(name string) (script *Script, err error) {
	script = &Script{
		Name: name,
		L:    luar.Init(),
		Log:  log.New(os.Stdout, name+" ", log.LstdFlags),
		Uuid: uuid.New(),
	}

	script.Seed()

	luar.Register(script.L, "", luar.Map{
		"_VERSION": "Glue 0.1",
	})

	err = script.loadLuaScript(name)
	if err != nil {
		err = script.loadMoonScript(name)

		if err != nil {
			log.Printf("%#v", err)
			return nil, errors.New("No such script " + name)
		}
	}

	return
}

func (s *Script) loadLuaScript(name string) error {
	err := s.L.DoFile(name)

	if err != nil {
		return err
	}

	s.Kind = "lua"

	return nil
}

func (s *Script) loadMoonScript(name string) error {
	contents, failed := ioutil.ReadFile(name)

	if failed != nil {
		return errors.New("Could not read " + name)
	}

	luar.Register(s.L, "", luar.Map{
		"moonscript_code_from_file": string(contents),
	})

	/*
		moonscript = require "moonscript"
		xpcall = unsafe_xpcall
		pcall = unsafe_pcall
		local func, err = moonscript.loadstring(moonscript_code_from_file)
		if err ~= nil then
			tetra.log.Printf("Moonscript error, %#v", err)
			error(err)
		end
		func()
	*/
	err := s.L.DoString(`moonscript = require "moonscript" xpcall = unsafe_xpcall pcall = unsafe_pcall local func, err = moonscript.loadstring(moonscript_code_from_file) if err ~= nil then tetra.log.Printf("Moonscript error, %#v", err) error(err) end func()`)
	if err != nil {
		s.Log.Print(err)
		return err
	}

	s.Kind = "moonscript"

	return nil
}

func (script *Script) Seed() {
	luar.Register(script.L, "glue", luar.Map{
		"goversion": runtime.Version,
	})

	luar.Register(script.L, "", luar.Map{
		"use": func(mod string) error {
			if impl, ok := modules[mod]; !ok {
				return fmt.Errorf("no such go module %s", mod)
			} else {
				luar.Register(script.L, mod, impl)
			}

			return nil
		},
	})

}

// Unload a script and delete its commands and handlers
func (s *Script) Unload(name string) error {
	s.L.Close()

	return nil
}

func byteSliceToString(slice []byte) string {
	return string(slice)
}
