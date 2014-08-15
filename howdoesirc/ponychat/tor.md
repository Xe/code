## Setting this all up on Linux

Thanks much to the anonymous user who wrote [this guide](http://pastebin.com/k8XHaABN), which I adapted into this tutorial.

Depending on what distribution of Linux you run, the process for installing Tor will vary. However, for Debian or Ubuntu boxes, the process for installing and setting up a basic Tor client is:

```
$ sudo apt-get update
$ sudo apt-get install tor
$ sudo service tor start
```

After that, open a user's IRC client as normal and instruct it to connect to the Tor Socks5 proxy at `127.0.0.1:9050`. I will show an example with Weechat (binary package `weechat` in most distributions) below.

After opening weechat, you need to add a Tor proxy:

```
/proxy add tor socks5 127.0.0.1 9050
```

Now, create a new server:

```
/server add PonyChat-Tor oabfdwrgyjxo7zn7.onion/6697
/set irc.server.PonyChat-Tor.ssl on
```

Set the proxy as Tor:

```
/set irc.server.PonyChat-Tor.proxy "tor"
```

Now you need to define a user's username and password for SASL authentication, otherwise a user's connection will not go through.

```
/set irc.server.PonyChat-Tor.sasl_mechanism dh-blowfish
/set irc.server.PonyChat-Tor.sasl_username "a user's_Account_Name"
/set irc.server.PonyChat-Tor.sasl_password "5up3rs3kr1+p@$$w0rD"
```

Finally, connect to PonyChat over Tor:

```
/connect PonyChat-Tor
```

For other operating systems such as Windows or OSX, you might have to do a little extra work, but there are many tutorials available for connecting to Freenode over Tor that can be easily adapted for PonyChat. However, it generally is the same process as above.

