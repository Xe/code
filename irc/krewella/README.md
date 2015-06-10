krewella
========

IRC notification bot based on HTTP requests

API
---

### `/message/:network/:channel`

#### POST

Requests the post body (as plain text) to be sent to :network's :channel

:channel will automatically have the proper channel name prefix as RFC 1459 
dictates.

Configuration
-------------

All configuration elements are in environment variables.

`KREWELLA_NETWORKS` -> comma-separated list of the networks that are defined 
below.

For each network:

`NICK` -> irc bot nickname
`USER` -> irc bot username
`HOST` -> irc server to connect to
`PORT` -> port to connect to on the irc server
