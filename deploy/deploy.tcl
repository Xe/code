#!/usr/bin/expect -f

# By Xe and Shockk

set host [lindex $argv 0]
set configpath [lindex $argv 1]
set motdpath [lindex $argv 2]

spawn ssh root@$host
set timeout 15
expect "(yes" {
	send "yes\r"
}

# Update system
send "apt-get update\r"
expect "~#"

send "apt-get upgrade --assume-yes > /dev/null\r"
expect "~#"

send "apt-get install build-essential libssl-dev flex bison git --assume-yes > /dev/null 2>&1\r"
expect "~#"

# Swapfile, 512MB on root
send "if \[ ! -f /swapfile1 \]\; then\r
dd if=/dev/zero of=/swapfile1 bs=1024 count=524288\r
mkswap /swapfile1\r
chown root:root /swapfile1
chmod 0600 /swapfile1
swapon /swapfile1
echo '/swapfile1 swap swap defaults 0 0' >> /etc/fstab\r
fi\r"

# Create ircd user
send "adduser ircd --gecos ircd --shell /bin/sh --disabled-password \r"

expect "~#"

send "su ircd\r"

expect "$"

set timeout 1500

# Set up elemental
send "cd\r"
send "if \[ -d elemental-ircd \]; then mv elemental-ircd elemental-ircd.old; fi\r"
send "git clone https://github.com/Elemental-IRCd/elemental-ircd.git\r"

# Compile elemental
send "cd elemental-ircd \r"
send "./configure \r"
send "make -j2\r"
send "make install \r"

# Configure elemental
send "cd \r"
send "cd ircd/etc \r"
send "wget $configpath/$host.conf \r"
send "mv $host.conf ircd.conf \r"
send "wget $motdpath/$host.motd \r"
send "mv $host.motd ircd.motd \r"
send "cd ..\r"

# Generate SSL certs
send "bin/genssl.sh\r"
send "US\r"
send "Virginia\r"
send "Ashburn\r"
send "ShadowNET IRC\r"
send "ircd ssl cert\r"
send "*.yolo-swag.com\r"
send "staff@yolo-swag.com\r"

# Start ircd
send "cd\r"

# Install flash policy daemon
send "git clone https://github.com/lyska/niifpd.git\r"
send "cd niifpd\r"
send "git submodule update --init\r"

# Start things on boot
send "echo \"@reboot /home/ircd/ircd/bin/ircd\\n@reboot (cd /home/ircd/niifpd; python niifpd.py)\" | crontab -\r"

# Drop to root
send "exit\r"
expect "~#"

# Start cron at boot
send "update-rc.d cron defaults\r"

expect "~#"

# End
send "reboot\r"
send "exit\r"

expect eof
