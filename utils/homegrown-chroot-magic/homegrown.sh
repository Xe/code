#!/bin/bash -x

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

# This pretty much uses chroot to emulate some of what Docker does on machines
# that can't run Docker.

function chroot_runcmd {
	CHROOT="$1"

	shift
	CMD=$*

	chroot ${CHROOT} ${CMD}
}

function mkalpinechroot {
	# Global variable abuse time!
	DESTDIR=$1
	ARCH=$2
	VERSION=$3

	wget "http://dl-3.alpinelinux.org/alpine/v2.5/main/${ARCH}/apk-tools-static-2.3.4-r0.apk"

	tar -xzf apk-tools-static-2.3.4-r0.apk

	mkdir -p ${DESTDIR}
	./sbin/apk.static -X "http://repos.lax-noc.com/alpine/${VERSION}/main" -U --allow-untrusted --root "${DESTDIR}" --initdb add alpine-base
	mkdir -p "${DESTDIR}/proc"

	rm *.apk
	rm -rf sbin

	mknod -m 666 "${DESTDIR}/dev/tty" c 5 0
	mknod -m 666 "${DESTDIR}/dev/full" c 1 7
	mknod -m 666 "${DESTDIR}/dev/ptmx" c 5 2
	mknod -m 644 "${DESTDIR}/dev/random" c 1 8
	mknod -m 644 "${DESTDIR}/dev/urandom" c 1 9
	mknod -m 666 "${DESTDIR}/dev/zero" c 1 5

	rm -f "${DESTDIR}/dev/null" && mknod -m 666 "${DESTDIR}/dev/null" c 1 3

	cp /etc/resolv.conf "${DESTDIR}/etc/"
	mkdir -p "${DESTDIR}/root"

	mkdir -p "${DESTDIR}/etc/apk"
	echo "http://repos.lax-noc.com/alpine/${VERSION}/main" >> "${DESTDIR}/etc/apk/repositories"
	echo "http://repos.lax-noc.com/alpine/${VERSION}/testing" >> "${DESTDIR}/etc/apk/repositories"

	mount --bind /proc "${DESTDIR}/proc"
}

# Super hacking time

if [[ "$EUID" -ne 0 ]]
then
	echo "You must be root" 2>&1
	exit -1
fi

if [ -e ./Chrootfile ]
then
	echo "Chrootfile found!"
else
	echo "No Chrootfile found in local directory :("
	exit -1
fi

function SetupChroot {
	if [[ "$DISTRO" -ne "alpine" ]]
	then
		echo "You mong. We only support Alpine"
		exit -1
	fi

	mkalpinechroot "$DESTDIR" "$ARCH" "$VERSION"
	chroot_runcmd "$DESTDIR" apk update
	chroot_runcmd "$DESTDIR" apk add "$NEEDPACKS"
}

function AddFile {
	cp "$1" "$DESTDIR$2"
}

function StartCMD {
	COMMAND=$*

	chroot_runcmd "$DESTDIR" $COMMAND
}

function MakeDir {
	mkdir -p "$DESTDIR$1"
}

# Run the Chrootfile

source ./Chrootfile

echo "chroot set up at $DESTDIR! Have fun!"

