#!/bin/bash

[ "$BASH_INTERACTIVE" ] || return

# dotfiles aliases
alias dt='dotfiles'
alias @='mux -s -d -w'

# general shell aliases
eval "$( dircolors -b )"
alias ls='ls --color --quoting-style=literal'
alias ll='ls -lFh'
alias l='ls -A'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias cd_='cd "$_"'
alias back='cd "$OLDPWD"'
alias cp='cp -v'
alias mv='mv -v'
alias rm='rm -v'
alias du='du -ch'
alias df='df -h'
alias free='free -h'
alias ln='ln -svfi'
alias mime='file -i'
alias killbg='kill -9 %1'
alias xcopy='xclip -selection clipboard -i'
alias xpaste='xclip -selection clipboard -o'
alias grep='grep -i --color=auto --exclude=.git'
alias egrep='grep -E'
alias fgrep='grep -F'
alias rgrep='grep -r'
alias ssh.direct='ssh -o ControlPath=none'
alias ssh.syncthing='echo "Opening Syncthing proxy in http://localhost:9393"; ssh.direct -NL 9393:localhost:8384'
alias fd='fdfind --hidden'
alias bat='batcat'
alias ffmpeg='ffmpeg -hide_banner'
alias ffprobe='ffprobe -hide_banner'
alias colortest='colortest-python -np'

# system administration
alias sctl='systemctl'
alias jctl='journalctl'
alias pstree='pstree -GUh'
alias ftrace='strace -fe trace=file'
alias ptrace='strace -fe trace=process'
alias psgrep='pgrep -fla'
alias pskill='pkill -fe'
alias smem='smem -akt'
alias watch='watch -cd -n 1 '
alias cpufreq='sudo watch "cpupower -c all frequency-info | grep assert"'
alias sleep-log='journalctl -u sleep.target --no-pager | grep ".*Stopped.*\|$"'
alias sleep-inhibit='systemd-inhibit sleep 999d'

# package management
alias pkget='aptitude -Z install'
alias pkgpurge='aptitude -Z purge'
alias pkgremove='aptitude -Z remove'
alias pkglist='dpkg -L'
alias pkgname='apt-cache pkgnames'
alias pkgsearch='apt-cache search'
alias pkgshow='command aptitude show'
alias pkginstall='sudo dpkg -i'

# development
alias vim='sensible-vim'
alias vi='vim'
alias vip='VIMCMD=vip sensible-vim'
alias vimdiff='vim -d'

alias lazyvim='NVIM_APPNAME=lazyvim vim'
alias lvi='lazyvim'

alias g='git'
alias e='git edit'

alias ssh-keygen-secure='ssh-keygen -o -t ed25519'

# mux aliases
for i in bundle console dev log migrate server watcher; do
  eval "alias @$i='mux @$i'"
done

# sudo aliases

# expand aliases in arguments
alias sudo='sudo '

# vim
alias suvi='sudo TMUX=$TMUX sensible-vim'
alias sudiff='suvi -d'

# file management
alias sucp='sudo cp -vi'
alias sumv='sudo mv -vi'
alias surm='sudo rm -v'
alias suln='sudo ln -svfi'
alias sush='sudo -i'
alias suown='sudo chown -vR root:root'
alias sumod='sudo chmod -vR 644'

# debian tools
alias apt='sudo apt'
alias apt-get='sudo apt-get'
alias aptitude='sudo aptitude'
alias dpkg-reconfigure='sudo dpkg-reconfigure'
alias update-alternatives='sudo update-alternatives'

# system tools
alias modprobe='sudo modprobe'
alias rmmod='sudo rmmod'
alias ethtool='sudo ethtool'
alias nft='sudo nft'
alias tcpdump='sudo tcpdump'
alias lsop='sudo lsof -Pni | grep --color=never LISTEN | egrep --color=auto "^[^ ]+|:\w+"'
