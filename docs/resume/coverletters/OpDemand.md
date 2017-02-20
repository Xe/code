Hello,

My name is Christine Dodrill and I would like to apply for your open position as 
a Linux Intern in Enterprise PaaS. I have been following Docker passively for 
a while now, but what sold me on it and what has made it part of my daily 
workflow was a few tools I had made on top of docker.



My first tool was a simple shell script to give me an "anonymous" Ubuntu 
install to play with. I'll put the function I wrote below:

```
function anonubuntu {
    docker run --rm -it ubuntu:trusty /bin/bash
}
```

This simple function has been invaluable over and over to give me a sandbox to 
play in that will go away when I close it. As I got more advanced with Docker, 
I started playing with the python bindings that DotCloud made for it and 
created a script that (if properly wrapped and leveraged) could end up being 
the core for a business infrastructure wrapped around selling slots for video 
game servers. The server I implemented around was for Minecraft due to the ease 
of setting it up. You may see the Dockerfiles and Python wrapper code (Python 
2.7 and requires Baker from pip) here: 
https://github.com/Xe/dotfiles/tree/master/dockerfiles/minecraft I use 
4 dockerfiles total to better separate out the parts of the service and server. 
The data dockerfile sets up a volume for the server to use, the data-ambassador 
sets up the permissions correctly in that volume, the server runs a standard 
copy of the Bukkit minecraft server, and the backup image creates a backup of 
an image and optionally automatically uploads it to Mega (requires mega.py from 
pip).

I would love this opportunity to help hammer out Deis and from what little 
experience I have had with it and similar tools (Flynn, Fig and others) I have 
been impressed by how simple it makes things. Docker is a fantastic technology 
and building on top of it makes so many previously difficult problems near 
trivial. I believe that my unique Linux server and desktop experience would be 
useful for feedback you would not otherwise get about Deis and could really 
make it better and easier to use.

Thank you for considering me for this position and I hope to hear back from you 
soon.

Christine Dodrill
+1 425 221 7761
sam@dodrill.net
