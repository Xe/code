# duta

#### A next generation IRC client protocol

By Christine Dodrill, 2014

## Apropos

This is a new client protocol designed to replace the standard RFC 1459 
protocol while still maintaining compatibility with the older protocol. The 
general format is identical to the TS6 link protocol. This is unfortunately 
a requirement because if the fundamental problems in the IRC client protocol.

Clients are expected to know information about:

 - Channels
 - Clients
 - Limited info about servers
 - Self information

## Channels

Channels have the following information:

 - Properties
   - params (metadata?)
 - A metadata hash table (string key, string value)
 - A topic object
 - An array of channel users
 - Timestamp
 - Lists:
   - Bans
   - Exceptions
   - Invexes
   - Quiets

### Ban-like Objects

 - Mask banned
 - timestamp
 - setter
 - reason
   - Can be Null

### Topic

Data:

 - Setter nick
 - time it was set
 - topic data

### Channel User

Data:

 - status prefixes
 - client UID or reference

## Clients

A client has the following:

 - Nickname
 - ident
 - display host
 - real name
 - UID
 - User Modes (not shown to users)
 - Is staff?
 - Account name or "\*"
 - Away status or None
 - Server id
 - IP address if the user is staff or 0.0.0.0
 - Metadata hash table
 - timestamp

Server side it adds:

 - Local socket
 - sendq / recvq
 - I:Line flags
 - oper permissions

## Servers

Servers have the following information:

 - SID
 - server name
 - server gecos

---

## Commands

The following commands are identical to their TS6 counterparts:

 - `PRIVMSG`
 - `NOTICE`
 - `NICK`
 - `CHGHOST`
 - `JOIN`
 - `PART`
 - `QUIT`
 - `KICK`
 - `KILL`
 - `SNOTE`
 - `SQUIT`
 - `SID` (s2s)
 - `AWAY`
 - `BAN` (s2s)
 - `CAPAB` (s2s)
 - `INVITE`
 - `JUPE` (s2s)
 - `KNOCK`
 - `MOTD`
 - `NICKDELAY` (s2s)
 - `PING`
 - `PONG`
 - `SASL` (s2s)
 - `SAVE` (s2s)

Numerics not replaced by a protocol verb are sent as is.

The following commands are modified:

 - `MLOCK` gets translated into a `PROPLOCK` with the same lock but in property 
 names instead of mode letters.

The following commands are removed:

 - `WALLOPS` messages from the uplink are replaced as `SNOMASK` `wallops`
   - Same with `LOCOPS`, `OPERWALL`,  and `GLOBOPS`

The following commands are original:

 - `CLIENT`
   - Marks information of a client that was previously unknown to the user.
   - `CLIENT <nick> <ident> <display host> <uid> <account name or *> <* if 
   staff, C if not> <TS> :<real name>`
   - Replaces NICK and USER for client registration
 - `CLINFO`
   - Returns a `CLIENT` line for a given UID.
 - `PROP`
   - Sets a channel property by name
   - `:<source> PROP <#channel> :<+/-prop_name> <+/-prop_name>`
 - `LIST`
   - Changes channel lists
   - `:<source> LIST <#channel> <list name> <action> <mask> <ts> <setter> [:reason]`
   - Ban reasons, setters and timestamps will not be broadcast to TS6 servers.
 - `STATUS`
   - Changes channel status (chanop, etc)
   - `:<source> STATUS <#channel> <ADD/REMOVE> <rank>`
   - `STATUS BURST`
    - Shows prefixes and UID's in the style of TS6 `SJOIN`.
    - `:<sid> STATUS <#channel> BURST <timestamp> :<[prefixes]uid> <[prefixes]uid>...`
 - `SNOMASK`
   - Allows opers to change their `SNOMASK`
   - `SNOMASK <ADD/REMOVE> :<+/-snomask_name>`

## Session Lifecycle

### Creation

A client initiating a duta session with a server starts by announcing its 
presence.

`>>> LOGIN <nickname> <ident> :<real name>`

If the client is wishing to authenticate over SASL, it will do the SASL session 
before sending a `LOGIN` command. After the server handshake is complete, the 
server will reply with a list of the settings, channel status flags, channel 
properties and user properties it supports.

### Channel Joining

A user sends a request to join a channel:

`>>> JOIN <#channel> [key if needed]`

The server sends a `CLIENT` line for every client in the channel that the user 
doesn't previously know about (weak checking by clients in adjacent channels) 
followed by a `STATUS BURST` line with the client statuses in standard TS6 
format. The channel `PROP`erties will be sent as well.

```
<<< :<sid> JOIN <yourid> <#channel>
<<< :<sid> CLIENT <nick> <ident> <display host> <account name> <staff?> :<real name>
<<< :<sid> STATUS <#channel> BURST <ts> :<[prefixes]uid> <[prefixes]uid>...
<<< :<sid> PROP <#channel> :<+-prop_name>...
```

### Private message from an unknown client

If a user gets a private message or notice from a client it doesn't previously 
knowledge about (services, random messages, etc) it must send a `CLINFO` line 
to the server requesting information on the client.

### Termination

A client `QUIT` or is `ERROR`'d by the server. A `QUIT` message will be sent to 
all clients in all adjacent channels the client was in.

