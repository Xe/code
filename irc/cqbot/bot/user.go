package cqbot

import "strings"

type User struct {
	Nick string
	User string
	Host string
}

// Split nick!user@host into a container
func NewUser(input string) *User {
	user := new(User)
	temp := strings.Split(input, "!")
	user.Nick = temp[0]
	temp = strings.Split(temp[1], "@")
	user.User, user.Host = temp[0], temp[1]

	return user
}
