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

KEYPATH="https://github.com/Xe.keys"

# Usage: initiate.sh <fqdn> <role>

HOSTNAME="$1"
ROLE="$2"

if [[ "$ROLE" -ne "drone" ]]
then
	echo "Non-drones are unsupported."
	exit -1
fi

# Update system
apt-get update
apt-get -y upgrade

# Install wget and git
apt-get -y install wget git-core

# Install SSH keys
cd ~
mkdir .ssh
cd .ssh
wget -O authorized_keys $KEYPATH
cd ~

# Add swap space
if [ ! -f /swapfile1 ]
then
	dd if=/dev/zero of=/swapfile1 bs=1024 count=524288
	mkswap /swapfile1
	chown root:root /swapfile1
	chmod 0600 /swapfile1
	swapon /swapfile1
	echo '/swapfile1 swap swap defaults 0 0' >> /etc/fstab
fi

# Install docker
wget -qO- https://get.docker.io/ | sh

# Clone unimatrix
git clone https://github.com/Xe/unimatrix

# Pull image
docker pull xena/adjunct

# Kick off adjunct
docker run -p 6667:6667 -p 6697:6697 --name="ircd" -e HOSTNAME="$HOSTNAME" -e ADJUNCT="go" -e ROLE="$ROLE" -d xena/adjunct /bin/sh /home/ircd/adjunct.sh

# Add cron entries
echo "@reboot docker start ircd" | crontab -

