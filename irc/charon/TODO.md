TODO for Project Charon
=======================

- [ ] ircd improvements
 - [ ] Attaching/detaching from a persistent client
 - [ ] Deduplicate code for protocol and client handling
 - [ ] quiets, ban exception, invex
 - [ ] invites
 - [ ] extbans
 - [ ] klines / other kinds of network level bans
 - [ ] /STATS
 - [ ] MOTD support
 - [ ] CAP support
 - [ ] SASL support
 - [ ] WEBIRC support
 - [ ] make redis-based data less fragile
  - [ ] forget about old data if a certain key hasn't been updated?
        Might become irellevant when attaching works

- [ ] Add protocol verbs:
 - [ ] KILL
 - [ ] CHANGENICK
       Think RSFNC
 - [ ] AKILL
 - [ ] ASSOCIATE
       Moves the "association" for a client from one server to another
 - [ ] AUTHENTICATE

- [ ] Write basic services
 - [ ] NickServ
   - [ ] LOGIN
   - [ ] REGISTER
   - [ ] GHOST
   - [ ] GROUP
   - [ ] LOGOUT
   - [ ] INFO
   - [ ] SASL support
 - [ ] ChanServ
   - [ ] REGISTER
   - [ ] XOP
    - [ ] VOP
    - [ ] AOP
   - [ ] DROP
   - [ ] INFO
 - [ ] OperServ
   - [ ] AKILL
   - [ ] INFO
   - [ ] SHUTDOWN

- [ ] Web client
 - [ ] See `doc/design/IRC IV - A New Hope.md`::`Changes to fundamental assertions`
 - [ ] Simple channel messages
 - [ ] Automatically `ATTACH` to a persistent session

- [ ] Websocket gateway

- [ ] Advanced services
 - [ ] Scrollback collector and replayer
 - [ ] Spam filtering service
 - [ ] Port some subset of Tetra to this
 - [ ] Slack-like integrations
