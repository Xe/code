package main

import "strings"

func helperExtractTerms(commandName string, messageText string) string {
	sentence := strings.SplitAfterN(messageText, commandName, 2)

	return sentence[1]
}
