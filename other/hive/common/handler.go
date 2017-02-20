package common

import (
	"fmt"
	"strings"
)

// GenerateHTTPChannelName takes http metadata and returns the best
// channel name to send a request to.
func GenerateHTTPChannelName(vhost, method, path string) string {
	thisHost := strings.Replace(vhost, ".", "%2e", -1)
	return fmt.Sprintf("http.%s.%s.%s", thisHost, method, path)
}

// ParseHTTPChannelName takes a channel name and breaks it apart into
// its components.
func ParseHTTPChannelName(channame string) (vhost string, method string, path string) {
	split := strings.SplitN(channame, ".", 4)

	return split[1], split[2], split[3]
}
