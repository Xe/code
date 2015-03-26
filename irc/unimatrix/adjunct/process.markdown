# Adjunct

## Basic system setup

1. Set up key-based authentication with the staff keyrings
2. Install docker on the host system
3. Clone unimatrix repository
4. Build docker image for elemental-ircd
5. Run docker image, forwarding all ports to the ircd container
6. Add docker image startup to startup process of server

## Docker image

1. Download configuration for irc daemon
2. Stamp configuration for irc daemon
3. Start IRC daemon
4. Start server local bot to listen for configuration updates
   - Server local bot opers up and initiates connection to its hub
5. Inform unimatrix of the server being online

## Server local bot

Listen for a rehash server notice and re-pull/stamp the configuration

