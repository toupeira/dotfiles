#!/bin/bash

. /etc/default/locale
export LANG LC_MESSAGES LC_NUMERIC LC_CTYPE

export PATH="/usr/sbin:/usr/bin:/sbin:/bin:/usr/local/sbin:/usr/local/bin:/usr/games"
[ -d ~/bin ] && export PATH="$HOME/bin:$PATH"
[ -d ~/.local/bin ] && export PATH="$PATH:$HOME/.local/bin"
[ -x /usr/libexec/path_helper ] && eval $( /usr/libexec/path_helper -s )

export EDITOR="vim"
command -v sensible-vim >/dev/null && export EDITOR="sensible-vim"

export PAGER="less"
export LESS="-iRMK"
export SYSTEMD_LESS="iRMKFX"

export PASSWORD_STORE_ENABLE_EXTENSIONS="true"
export PGDATABASE="postgres"
export RIPGREP_CONFIG_PATH=$HOME/.config/ripgrep.conf

export QT_AUTO_SCREEN_SCALE_FACTOR=0
export QT_QPA_PLATFORMTHEME="gtk2"
export CALIBRE_USE_DARK_PALETTE=1

export GOPATH="$HOME/src/upstream/go"

# Colorize manpages
export LESS_TERMCAP_mb=$'\e[1;31m'
export LESS_TERMCAP_md=$'\e[1;32m'
export LESS_TERMCAP_us=$'\e[1;33m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_ue=$'\e[0m'

[ -n "$SSH_CONNECTION" ] && export GPG_TTY=$( tty )
[ -x /usr/bin/phantomjs ] && export PHANTOMJS_BIN="/usr/bin/phantomjs"
command -v chromium &>/dev/null && export CHROME_BIN="chromium"
