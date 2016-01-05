package cqbot

import "errors"
import "code.google.com/p/go-uuid/uuid"

type Handler struct {
	Uid    string
	Script *Script
	impl   func(*Line)
	verb   string
}

// Add a handler function for a protocol verb.
func (bot *Bot) AddHandler(verb string, impl func(line *Line)) (handler *Handler, err error) {
	handler = &Handler{
		Uid:  uuid.New(),
		impl: impl,
		verb: verb,
	}

	if _, ok := bot.handlers[verb]; !ok {
		bot.handlers[verb] = make(map[string]*Handler)
	}

	bot.handlers[verb][handler.Uid] = handler

	return
}

// Delete a handler from the list of handlers.
// TODO: allow users to remove a specific handler from a verb
func (bot *Bot) DelHandler(verb string, uid string) (err error) {
	if _, present := bot.handlers[verb]; !present {
		return errors.New("No such verb to delete handler")
	}

	delete(bot.handlers[verb], uid)

	bot.Log.Printf("Deleted Handler for %s:%s", verb, uid)

	return nil
}
