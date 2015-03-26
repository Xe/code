# Duta

Duta is a messenger application that uses IRC as its communication backend.

A mockup:

![](http://puu.sh/dpu49/fbf188b3cd.png)

To Build
--------

Install grunt if you haven't already

```
npm install -g grunt-cli
```

Then run the following to download version 0.12.2 of atom-shell
```
cd ./build
npm install
grunt download-atom-shell
```

Then you should be able to run the app:

```
./build/atom-shell/Atom.app/Contents/MacOS/Atom ./duta
```

Instructions will differ for operating systems that are not OSX.
