tomato-cheese
=============

Command tomato-cheese is a websocket -> irc daemon proxy.

It is very simple and  is intended for deployment on the actual machine that is 
serving IRC traffic. This will trivially allow IRC network administrators to 
let any clients connect over websockets instead of using Mibbit, KiwiIRC, or 
other solutions such as using Flash as a TCP socket or LightIRC.

Usage is simple:

```console
$ ./tomato-cheese -h
Usage of ./tomato-cheese:
  -conf="./cheese.conf": config location
  -debug=false: enable debugging output?
```

If using WEBIRC, be sure to set up the correct I:Line on your irc daemon. See 
your IRC daemon's configuration for more information. Users using 
Elemental-IRCd can use one based on the following:

```
/* Example WEBIRC authblock */
auth {
    /* user: webirc@IP.OF.YOUR.WEBIRC . the webirc@ part is required */
    user = "webirc@127.0.0.1";

    /* password: password the webirc client sends in the WEBIRC command.
     * You can use a encrypted password here.
     */
    password = "fish";

    /* spoof: This is required to keep it what it is currently if you
     * want the webirc client to show the users' real host as their
     * host on IRC.
     */
    spoof = "webirc.";

    class = "users";
    flags = need_ssl, no_tilde;
};
```

A configuration file is also needed. The structure of it is in 
`cheese.conf.example`

Start tomato-cheese with your favorite process manager and then sit back and 
relax.

---

For help please open an issue or join `irc.ponychat.net` `#geek` and ask for 
Xena.
