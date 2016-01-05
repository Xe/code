Proposal for fixing IRC for the skype era
=========================================

#### IRC IV - A New Hope

![]( https://pbs.twimg.com/media/CCArr76W8AEdGdA.png )

IRC as it is sucks. It has many many protocol level failures that more modern chat implementations fix in ways better than modern IRC daemons do.

This proposal is an idea on how to fix all this.

1. Split away a lot of the ephemeral nature of irc connections
2. Allow a mechanism for storing "scrollback" of past conversations
3. Simplify privacy settings and small group creation
4. For extra credit bake in WebRTC
5. Multimedia support memes
  5.5. dank memes
6. Batch file sharing
7. Contacts / "friends list"
8. Built-in spam protection of any kind
9. TLS requirement
10. Remove AMSG and AWAY announcements

Terms
-----

"must" means anything that is a hard requirement. Any service or client failing to implement this feature is to be not considered following this specification

Changes to fundamental assertions
---------------------------------

- One client in one channel will no longer be one TCP connection
  -  part of the ephemeral / persistent split
- Move ident field to user metadata rather than part of the core identification of a user
- Split up MODE into mutliple commands
  - ACCESS
- Line length is 2048 characters
  -  Regardless of tags / body
- Flood restrictions do not apply to private groups
- Connections over plain text should have very limited capabilities
  - must not be allowed to log into services
  - must not be allowed to view user information
  - must not be allowed to join more than a few channels
  - must not be allowed to gain any status in any channel
  - must not be allowed to create channels
  - must not be allowed to have private conversations
  - must be told on connection that they are connected via plain text and should consider connecting over SSL
    - consider removing support completely? Any existing client could connect over IRC via a plaintext-to-TLS gateway.
- Bans should have reasons
- Ban lists should be infinite for public channels

Client requirements
===================

Backwards compatability
-----------------------

- All of the previous meanings of MODE will be maintained indefinitely.
- All new features must be opt-in.
- New capabilities must be negotiated before used

Contacts
--------

- Contact groups
- Notify all in contacts (bulletin/status update/change)
- Showing clients in channels by a "real name" instead of a nickname for members of the "friends list"

Attaching to a persistent "session"
-----------------------------------

Clients must implement:
- sasl
  - the changeover must be mostly invisible to the user and to other users

Clients on the network will be in one of two states:
1. Attached
2. Detached

This will function similar to how ZNC works, except it will reserve names at the network level. Clients that are in a "detached" state will show as such on a WHOIS lookup.

When a client attaches to an existing session, it will recieve JOIN lines for every channel the existing session is in. This will appear to bypass channel limits, join speed limits, invite requirements, bans or the like because no JOIN will actually be made. The client will also be sent a NICK message signifying that its nickname is being changed to the nickname of the persistent client.

Scrollback
----------

Clients must implement:
- server-time
  - sends the network's time with each message (it will /have/ to be synchronized)
- echo-message
  - sends all outgoing PRIVMSG/NOTICE lines back to the user
- message tags

Any clients using the scrollback recording feature must connect over ssl. Any attempts to use or query any scrollback information over an insecure channel must be denied.

Server requirements
===================

Additions
---------

- Allow a JSON api to:
    - control channels
    - control groups
    - query log scrollback

Gateway servers must support websockets. No excuses.

User age restriction should be a core part of the service:
    - servers must not store logs of users that are underage
    - servers must not let an underage user join a "mature" channel
    - servers must allow channels to mark themselves as "mature" or not

Spam protection
---------------

Public channels should have the option to set patterns that once matched will trigger a kick from the channel.

Also: Bayesian filtering

Spam protection must never apply to private messages

Channel access / Privacy
------------------------

- Remove FLAGS
- Only allow one "founder" of a channel

Segregate away the ephemeral nature of IRC
------------------------------------------

Things to be addressed:
- hostmasks
  - hostmask/user/nickname should be consolidated into a single identifier; there is no benefit to having the overhead e.g. ICQ has a single number for login, Skype has a username, DHT protocols use a single public key, etc.
  - if you had a user identifier such as with the account tag, you could completely drop the prefix even when passing messages between servers
- bans
    - channel level
    - network level
- bad documentation

                 --> Ocean_View (NightGuard@49-553-500-653.res.bhn.net) has joined #help
        (Ocean_View) Excuse me, but how do I set an OP? The FAQ is too vague

### Hostmasks

Hostmasks in this new model will either represent:
1. an account
2. an ephemeral connection

### Bans

This is where it gets hairy.

Channels can explicitly disallow ephemeral connections in their rooms. == of chmode +r. The default setting will be with this set on.

Channels may explicitly opt-out of any logs of that channel being stored in shared scrollback. This must be a permanent setting.
- Changing this setting may only be done by the channel owner
- This setting must have a confirmation to be changed
- This setting must not be able to be changed more than once

Also IPv6 sucks.

Good things to take away from IRC
---------------------------------

1. Non-centralized one-to-many routing
2. Two or three kinds of channel access control
  - modes  (ircd)
  - flags  (atheme)
  - akick  (atheme)
  - access (atheme)
  - xop    (atheme)
  - group based (atheme)
3. channel modes
Most of the modes that matter:
  - +r (need registration)
  - +t (ops only topic)
  - +m (need voice to speak)
  - +c (color stripping without channel access)
  - +g (free invite)
  - +z (op moderated)
  - +S (SSL only)
  - +f (forward)
  - +j count:time (pace join)
4. single commands doing multiple things
  - MODE
5. Completely regular (as in regular language) protocol that can be parsed in proportional time to the message length (with constant 1)!
  -  Capn' Proto!

Linking
-------

- RabbitMQ
  - language agnostic
- ZMQ
  - language agnostic
- Redis Pub/Sub
  - language agnostic
- Hazelcast
  - Not language agnostic (JVM) and appears to be mostly pay software

Effectively we are just moving data from one machine to another in subscription channels. This is not rocket science. It would be nice to have this be in a language-agnostic method as to not lock network administrators into one way of doing things. Yep, channels over channels. Yo dawg.

### Structure

A message has:
- a source
- a destination
- tags
- a command verb
- arguments

Scrollback
----------

Can be queried from the ircd
- sync backlog across multiple clients
 - clients with differing capabilities?
  - have a pseudoclient like LogServ for users not running a client compatible with the new way of doing things
  - Maybe also take a leaf out of ZNC's backlog/privmsg backlog features?
  - consistency across servers?
- sync joined channels across clients and sessions

Candidates for storage:
- plain old SQL database
- RethinkDB has cool push things
  - I don't think the push things would be useful.
- ElasticSearch

Network operators must not have access to user scrollback messages under normal circumstances. Any time the network operators must have access to such records (subpoena, etc), there must be a note left for the user to indicate that an access had been made.

Messages in public channels should be shared across all accounts for storage efficiency.

Public channels should have an option to have a public scrollback page for people to reference.

Message retention should be at least a year.
