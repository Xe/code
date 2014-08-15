#filescan
---------

Makes a log of all files that start with an underscore created in /home

Comes with an initscript

## Installation

1. Install debian package `python-pyinotify` or the equivalent on your distro
2. Make a user called `filescan` with home `/var/lib/filescan`; ensuring `filescan` can write to its home directory
3. Copy `filescan.py` to `/var/lib/filescan`
4. Create `/var/log/newfiles.log` and chown it to `filescan`
5. Install initscript to `/etc/init.d`
6. `service filescan start`
7. Add it to the default services if you like
