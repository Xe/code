Asparagus
=========

An IRC bot in Python

Right now this is still very in development, but it is planned to be a fully
featured IRC bot comparable with Skybot and other python IRC bots.

### Features

 - Asynchronous socket i/o
 - Basic MPD client
 - Brainfuck interpreter
 - Decsion maker
 - Dynamic lookups based on what is linked:
   - Twitch.tv streams
   - Youtube videos
 - Help command
 - Easy to read json configuration file
 - Memoising fibbonacci number generator
 - Weather lookups:
   - http://thefuckingweather.com
   - World Weather Online (needs API key)

### Python dependencies:

 - beautifulsoup4
 - python-mpd2
 - requests

The virtualenv wrapper/launcher *should* take care of these for you, but
sometimes it doesn't.

### Supported IRC daemons:

 - Charybdis
 - snircd

Other daemons may work, but if there is something retarded like a numeric
conflict, you are on your own.

### Setup

1. Run `git submodule update --init` to grab the latest version of `niilib`
2. Copy `etc/config.json/example` to `./config.json`
3. Edit configuration settings as needed
4. Run `./asparagus` and see if it works
5. Enjoy your new IRC bot

### Support

The official channel for this bot is `irc.yolo-swag.com` `#asparagus`, come
by and say hello!



[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/lyska/asparagus/trend.png)](https://bitdeli.com/free "Bitdeli Badge")

