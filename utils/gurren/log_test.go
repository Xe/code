package gurren

import "testing"

const URL = "http://127.0.0.1:9200"
const Index = "test"

func TestCreateClient(t *testing.T) {
	_, err := New([]string{URL}, Index)

	if err != nil {
		t.Fatal(err)
	}
}

func TestLogData(t *testing.T) {
	l, _ := New([]string{URL}, Index)

	err := l.Log("Oh hi there lol")

	if err != nil {
		t.Fatal(err)
	}
}

type Foo string

func (f Foo) String() string {
	return string(f)
}

func TestLogStringer(t *testing.T) {
	l, _ := New([]string{URL}, Index)

	f := Foo("hi from TestLogStringer")

	err := l.Log(f)

	if err != nil {
		t.Fatal(err)
	}
}

type Bar struct {
	Foz  string `json:"foz"`
	Broz int    `json:"broz"`
}

func TestLogJsonStruct(t *testing.T) {
	l, _ := New([]string{URL}, Index)

	b := &Bar{
		Foz:  "This is a foz logging line",
		Broz: 42,
	}

	err := l.Log(b)

	if err != nil {
		t.Fatal(err)
	}
}
