#!/bin/bash

mkdir /tmp/gpg-install
cd /tmp/gpg-install

wget https://kojipkgs.fedoraproject.org//packages/gnupg2/2.1.4/1.fc23/x86_64/gnupg2-2.1.4-1.fc23.x86_64.rpm
wget https://kojipkgs.fedoraproject.org//packages/gnupg2/2.1.4/1.fc23/x86_64/gnupg2-smime-2.1.4-1.fc23.x86_64.rpm
wget https://kojipkgs.fedoraproject.org//packages/gnupg2/2.1.4/1.fc23/x86_64/gnupg2-debuginfo-2.1.4-1.fc23.x86_64.rpm

dnf -y install ./*.rpm
dnf versionlock gnupg2-2.1.4-1.fc23.x86_64

touch /var/local/gpg-is-installed-do-not-fear
