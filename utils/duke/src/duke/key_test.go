package duke

import (
	"io/ioutil"
	"testing"
)

const (
	username   = "xena"
	pubkeyFile = "./duketest.pub"
)

func TestNewKey(t *testing.T) {
	data, err := ioutil.ReadFile(pubkeyFile)
	if err != nil {
		t.Fatal(err)
	}

	k := NewKey(username, string(data))

	expected := []struct {
		input, output string
	}{
		{username, k.Username},
		{"ssh-rsa", k.Kind},
		{"AAAAB3NzaC1yc2EAAAADAQABAAABAQDTDQXzFma3+C43Wygo6YQnhL9gC5jIWTS+gWC7xxjX+WKStHTRwksXv1rOFuZzdmGp/iKGF1Nf2HKjMvxXgidMdfLZ642nDuNXSmTgd9qiLdLAfaYOveFPgWYnk9glGudfHMtB1KNnJvxzgXf4O+9eF0q9v2cOQJHefDAKq03yDhJ27ERN+KU/dIAI/b1FF4OZxw9YyF60lD/ySoH5hwIpi2A0H/1D9Usku1GFXzxQ8TMWfCWM47VhhpQ0+LgEKXbYa61Mblqz1MTWQYWVGT5qjx1EhA+pfsaka3H9J0OPgvrOHh1lEJTMWWDX3BfUSY41iikZr6uutIzpXGNZ6K91", k.PublicKey},
		{"xena@fluttershy", k.Comment},
		{"a9:b9:b1:1a:95:32:9f:82:ab:d5:b3:ca:e3:8b:56:a5", k.Fingerprint},
	}

	for _, tCase := range expected {
		if tCase.input != tCase.output {
			t.Fatalf("expected %q, got %q", tCase.input, tCase.output)
		}
	}
}
