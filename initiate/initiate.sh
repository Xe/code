#!/bin/sh

scp niichan@volantis.yolo-swag.com:/home/niichan/ssh.tgz .
tar zxf ssh.tgz

# Setup packages
sudo apk add bash zsh ca-certificates alpine-sdk flex bison
sudo apt-get install zsh build-essential flex bison
sudo pacman -Syu zsh git flex bison gcc make autoconf

# Clone dotfiles and dependencies
mkdir code
cd code
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git
git clone git@github.com:Niichan/dotfiles
cd dotfiles

# Install oh my zsh
wget --no-check-certificate https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | bash

function setlink
{
        ln -s `pwd`/$1 $HOME/$1
}

rm ~/.zshrc

#set links
setlink .profile
setlink .zshrc
setlink .zsh
setlink .vim
setlink .vimrc
setlink .cheat
setlink .gitconfig
setlink .rtorrent.rc
setlink .Xmodmap

