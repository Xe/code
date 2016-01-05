/*
 * Copyright (C) 2014 Christine Dodrill <xena@yolo-swag.com> All rights reserved.
 *
 * This software is provided 'as-is', without any express or implied
 * warranty. In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 *
 * 2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 *
 * 3. This notice may not be removed or altered from any source
 *    distribution.
 *
 */

package cqbot

import (
	"errors"
)

// Wrapper struct around a command implementation
type Command struct {
	ArgcMin int
	ArgcMax int
	Verb    string
	Help    string
	Func    func(*User, []string) string
}

// Create a command and allow outer handlers to do error checking.
func (bot *Bot) AddCommand(verb string, help string, argcmin int, argcmax int, impl func(*User, []string) string) (command *Command, err error) {
	if _, present := bot.Commands[verb]; present {
		return nil, errors.New("Duplicate command " + verb + " tried to be added!")
	}

	command = &Command{
		Verb:    verb,
		Help:    help,
		ArgcMax: argcmax,
		ArgcMin: argcmin,
		Func:    impl,
	}

	bot.Commands[verb] = command

	return
}

// Delete a command with name verb
func (bot *Bot) DelCommand(verb string) (err error) {
	if _, present := bot.Commands[verb]; !present {
		return errors.New("No such verb to delete handler for " + verb)
	}

	delete(bot.Commands, verb)

	return nil
}
