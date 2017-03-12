Message Router
==============

High Level
----------

Adaptor >>= Router >>= Handler >>= Adaptor

- A Handler has Communities it subscribes to Events from
- An Adaptor exposes one or more communities and sends in/out events

Limitations of the Current System
---------------------------------

- Handlers cannot independently send messages to adaptors outside of recieving one
- Handlers do not time out and stay in redis eternally

Protocol
--------

### Adaptors

Adaptors will still pipe messages to `betterbot.input`, but inestead of sending a
`betterbot.birth` event, it will send a `betterbot.adaptor.birth` also include the
following information (to be stored in redis or something):

- List of communities the adaptor exposes (guilds in discord, networks on IRC, bot connections
  on telegram, etc and their input queues)
- List of message groups it knows about (human-readable name from the protocol -> protocol ID
  for API calls)

### Handlers

Command or event handlers will listen on their input queue and push messages to the
requested output queue. Usually this will be sending another message back to the adaptor
it came from. Instead of sending a `betterbot.birth` message, it will send a
`betterbot.handler.birth` message that will contain the following:

- List of communities that the handler will subscribe to
- List of `;`/`/`-commands that the handler exposes (messages will then be sent to
  `betterbot.command.{commandname}`)
- List of regex matches that should trigger a message to be sent to the handler's
  `input` queue
- If the handler is exclusive ("MC Hammer" mode), set this flag to true

The router should allow multiple instances of each handler to run while only allowing
one of each kind to reply to a message.

### Handlers Independently Reaching Out to Adaptors

If a handler wants to reach out to an adaptor and send a message to it, the handler should
send the following to the router on channel `betterbot.unsolicited.handler.input` with
the following data in a `*common.Message` struct:

- The target protocol, community and human-readable group ID
- All other normal message fields

The adaptor will reply with either a string representing the error or a string containing
"okay".
