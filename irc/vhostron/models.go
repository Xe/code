package main

import (
	"time"

	"github.com/Xe/uuid"
)

type Confirmation struct {
	Domain    string `json:"domain"`
	Requester string `json:"requester"`
	ID        string `json:"uuid"`
	Date      string `json:"date"`
}

func NewConfirmation(domain, requester string) (c *Confirmation) {
	t := time.Now()
	id := uuid.New()

	return &Confirmation{
		Domain:    domain,
		Requester: requester,
		Date:      t.Format(time.RFC3339),
		ID:        id,
	}
}
