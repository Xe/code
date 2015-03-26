# Copyright (C) 2014 Christine Dodrill <xena@yolo-swag.com> All rights reserved.
# 
# This software is provided 'as-is', without any express or implied
# warranty. In no event will the authors be held liable for any damages
# arising from the use of this software.
# 
# Permission is granted to anyone to use this software for any purpose,
# including commercial applications, and to alter it and redistribute it
# freely, subject to the following restrictions:
# 
# 1. The origin of this software must not be misrepresented; you must not
#    claim that you wrote the original software. If you use this software
#    in a product, an acknowledgment in the product documentation would be
#    appreciated but is not required.
# 
# 2. Altered source versions must be plainly marked as such, and must not be
#    misrepresented as being the original software.
# 
# 3. This notice may not be removed or altered from any source
#    distribution.
# 

# We are good to go now

# Only set up a client leaf for now
rm -f /home/ircd/ircd/etc/ircd.conf
rm -f /home/ircd/ircd/etc/ircd.motd
wget -O /dev/null http://unimatrix.yolo-swag.com:5000/server-register
wget -O /home/ircd/ircd/etc/ircd.conf http://unimatrix.yolo-swag.com:5000/confs/
wget -O /home/ircd/ircd/etc/ircd.motd http://unimatrix.yolo-swag.com:5000/motds/shadownet/"$HOSTNAME"

openssl req -new -nodes -sha512 -out ssl.csr -keyout ssl.key -config shadownet.sslconf
openssl x509 -req -sha512 -days 365 -in ssl.csr -signkey ssl.key -out ssl.crt
openssl dhparam -out dh.pem 2048
cp ssl.??? ircd/etc
cp dh.pem ircd/etc
rm -f /home/ircd/ircd/etc/ssl.csr

# Kick off ircd
/home/ircd/ircd/bin/ircd -conftest
/home/ircd/ircd/bin/ircd -foreground -configfile /home/ircd/ircd/etc/ircd.conf
# Needs to be -foreground so docker doesn't kill the container

