Duke command line tool
======================

- Normal user management
- create an ephemeral signed ssh key for a user and after a human approval, add it to their SSH agent and to the target machine
  - https://godoc.org/golang.org/x/crypto/ssh/agent
  - maybe some client to the central server checking a ssh public key for validity instead of modifying the authorized_keys file on the host only for ephemeral logins
