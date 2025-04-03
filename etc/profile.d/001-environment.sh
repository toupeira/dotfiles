#!/bin/bash

export PATH="/usr/sbin:/slack/dotfiles/bin:/usr/bin:/usr/local/sbin:/usr/local/bin:/usr/games"
[ -d ~/bin ] && export PATH="$PATH":~/bin

# terminal apps
export TERMINAL_EMULATOR="x-terminal-emulator"
export EDITOR="sensible-vim"
export PAGER="less"
export LESS="-iRMK --mouse"
export SYSTEMD_LESS="iRMKFX --mouse"
export RIPGREP_CONFIG_PATH=~/.config/rg/config
export BAT_THEME="TwoDark"
export WEZTERM_SHELL_SKIP_ALL=1

[ "$TERM" = "tmux-256color" ] && export COLORTERM=${COLORTERM:-truecolor}

# desktop apps
export GTK_A11Y=none
export NO_AT_BRIDGE=1
export QT_AUTO_SCREEN_SCALE_FACTOR=0

export CALIBRE_USE_DARK_PALETTE=1
export CHROMIUM_FLAGS="--password-store=basic --enable-features=VaapiVideoDecodeLinuxGL,VaapiVideoEncoder"

# colorize manpages
export LESS_TERMCAP_mb=$'\e[1;31m'
export LESS_TERMCAP_md=$'\e[1;32m'
export LESS_TERMCAP_us=$'\e[1;33m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_ue=$'\e[0m'
export GROFF_NO_SGR=1

# initialize mise
if [ "$HOSTNAME" != "snafu" ]; then
  export MISE_ENV='development'
  [ "$TMUX" ] && tmux set-environment MISE_ENV "$MISE_ENV"
fi

if [ "$BASH_INTERACTIVE" ]; then
  eval "$( mise activate bash )"
else
  eval "$( mise activate --shims )"
fi
