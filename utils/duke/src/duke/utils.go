package duke

import (
	"crypto/md5"
	"encoding/base64"
	"fmt"
	"log"
)

// getFingerprint takes an SSH key in and returns the fingerprint (an MD5 sum)
// and adds the needed colons to it.
func getFingerprint(key string) string {
	h := md5.New()

	data, err := base64.StdEncoding.DecodeString(key)
	if err != nil {
		return "error"
	}

	h.Write(data)

	ret := addColons(fmt.Sprintf("%x", h.Sum(nil)))

	if key == ret {
		log.Fatal("Assertion failure")
	}

	return ret
}

// addColons adds colons every second character in a string to make the formatting
// of a public key fingerprint match the common standard:
//
//    dd3bb82e850406e9abffa80ac0046ed6
//
// becomes
//
//    dd:3b:b8:2e:85:04:06:e9:ab:ff:a8:0a:c0:04:6e:d6
func addColons(s string) (r string) {
	if len(s) == 0 {
		return ""
	}

	for i, c := range s {
		r = r + string(c)
		if i%2 == 1 && i != len(s)-1 { // Even number, add colon
			r = r + ":"
		}
	}

	return
}
