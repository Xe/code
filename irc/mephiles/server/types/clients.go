package types

import (
	"log"
	"strings"
)

// Clients is a struct that holds all of the clients that this server knows
// about.
type Clients struct {
	ByNick  map[string]*Client // By nickname
	ByTS6ID map[string]*Client // By the TS6 UID
	ByUUID  map[string]*Client // By the RFC uuid
}

// AddClient adds a Client to the Clients structure.
func (c *Clients) AddClient(client *Client) {
	c.ByNick[strings.ToUpper(client.Nick)] = client
	c.ByTS6ID[client.TS6ID] = client
	c.ByUUID[client.UUID] = client
}

// DelClient deletes a Client from the Clients structure.
func (c *Clients) DelClient(client *Client) (err error) {
	delete(c.ByNick, strings.ToUpper(client.Nick))
	delete(c.ByTS6ID, client.TS6ID)
	delete(c.ByUUID, client.UUID)

	return
}

// ChangeNick changes a client's nickname and updates the ByNick map.
// This does not check if the nickname already exists. Other parts need to
// do this.
func (c *Clients) ChangeNick(client *Client, newnick string) (err error) {
	if _, present := c.ByNick[strings.ToUpper(client.Nick)]; !present {
		log.Fatal("wtf")
	}

	delete(c.ByNick, client.Nick)

	c.ByNick[strings.ToUpper(newnick)] = client

	return
}
