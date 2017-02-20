package gurren

import (
	"errors"
	"fmt"
	"os"
	"path"
	"reflect"
	"time"

	"github.com/Xe/uuid"
	"gopkg.in/olivere/elastic.v2"
)

// ErrCantInsertLog is an error for when an index for whatever reason
// is not inserted but elastic doesn't throw an error back.
var (
	ErrCantInsertLog = errors.New("gurren: could not insert index")
)

// Message is a log message.
type Message struct {
	Data     interface{} `json:"data"`
	Date     string      `json:"date"`
	Program  string      `json:"program"`
	Type     string      `json:"type"`
	Hostname string      `json:"hostname"`
}

// unformattedMessage is a line of unformatted logging data
type unformattedMessage struct {
	Text string `json:"text"`
	Kind string `json:"kind"`
}

// Logger is the interface into sending logging data in.
type Logger struct {
	conn  *elastic.Client
	index string
}

// Log sends arbitrary data to Elasticsearch.
//
// If you send this a string or a fmt.Stringer, it will be wrapped as an UnformattedMessage.
func (l *Logger) Log(data interface{}) (err error) {
	m := &Message{
		Date:    time.Now().Format(time.RFC3339),
		Program: path.Base(os.Args[0]),
		Type:    reflect.TypeOf(data).String(),
	}

	hostname, err := os.Hostname()
	if err != nil {
		return err
	}

	m.Hostname = hostname

	switch data.(type) {
	case string:
		myData := data.(string)

		um := &unformattedMessage{
			Kind: "unformatted",
			Text: myData,
		}

		m.Data = um

	case fmt.Stringer:
		myData := data.(fmt.Stringer)

		um := &unformattedMessage{
			Kind: "unformatted",
			Text: myData.String(),
		}

		m.Data = um

	default:
		m.Data = data
	}

	rep, err := l.conn.Index().
		Index(l.index).
		Type("gurren").
		Id(uuid.New()).
		BodyJson(m).Do()

	if err != nil {
		return err
	}

	if !rep.Created {
		return ErrCantInsertLog
	}

	return nil
}

// New creates a new Logger and returns it or an error.
func New(urls []string, index string) (l *Logger, err error) {
	l = &Logger{
		index: index,
	}

	c, _ := elastic.NewClient(elastic.SetURL(urls[0]))
	l.conn = c

	_, _, err = c.Ping().Do()
	if err != nil {
		return nil, err
	}

	exists, err := c.IndexExists(index).Do()
	if err != nil {
		return nil, err
	}

	if !exists {
		ci, err := c.CreateIndex(index).Do()
		if err != nil {
			return nil, err
		}

		if !ci.Acknowledged {
			return nil, fmt.Errorf("Could not create index %s", index)
		}
	}

	return
}
