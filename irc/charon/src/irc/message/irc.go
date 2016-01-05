package message

import (
	"fmt"
	"strings"
)

type Message struct {
	Tags   map[string]string `json:"tags"`
	Source string            `json:"source"`
	Verb   string            `json:"verb"`
	Args   []string          `json:"args"`
}

func ParseMessage(line string) (message *Message) {
	fields := strings.Split(line, " ")
	message = &Message{}
	message.Tags = make(map[string]string)
	index := 0

	if strings.HasPrefix(fields[0], "@") {
		field := fields[0][1:]
		tags := strings.Split(field, ";")

		for _, tag := range tags {
			if strings.Contains(tag, "=") {
				sides := strings.SplitN(tag, "=", 2)
				message.Tags[sides[0]] = sides[1]
			} else {
				message.Tags[tag] = "true"
			}
		}

		index++
	}

	if strings.HasPrefix(fields[index], ":") {
		message.Source = fields[index][1:]
		index++
	}

	message.Verb = strings.ToUpper(fields[index])
	index++

	for i := index; i < len(fields); i++ {
		field := fields[i]

		if strings.HasPrefix(field, ":") {
			message.Args = append(message.Args, strings.Join(fields[i:len(fields)], " ")[1:])
			break
		}
		message.Args = append(message.Args, field)
	}
	return
}

// String returns the serialized form of a RawLine as an RFC 1459 frame.
func (r *Message) String() (res string) {
	if r.Source != "" {
		res = res + fmt.Sprintf(":%s ", r.Source)
	}

	res = res + fmt.Sprintf("%s", r.Verb)

	for i, arg := range r.Args {
		res = res + " "

		if i == len(r.Args)-1 { // Make the last part of the line an extparam
			res = res + ":"
		}

		res = res + arg
	}

	return
}
