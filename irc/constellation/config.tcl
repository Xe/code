# settings.tcl file for servers

# username to ssh in as, user account to set up
set username root
set ircduser ircd

# path to git repository
set gitrepo "https://github.com/Elemental-IRCd/elemental-ircd.git"

# Debian build dependencies
set builddeps "apt-get install libssl-dev bison flex build-essential git python-dev libxml2-dev libxslt-dev python-pip python-virtualenv"
# For Alpine Linux
#set builddeps "apk add openssl-dev alpine-sdk flex bison"

set foldername elemental-ircd

# IRCd settings
set prefix "/home/xena/prefix/deploytest-ircd"
set nicklen "31"

# Build flags
set flags "--prefix=$prefix --with-nicklen=$nicklen"

