#!/bin/bash
# shellcheck disable=SC2034

[ "$BASH_VERSION" ] && [ "$PS1" ] && BASH_INTERACTIVE=1
[ "$BASH_INTERACTIVE" ] && shopt -q login_shell && BASH_LOGIN=1
[ "$BASH_INTERACTIVE" ] || return

# Helper to check for commands
has() {
  command -v "$1" &>/dev/null
}

# Shell options
shopt -s autocd
shopt -s extglob
shopt -s globstar
shopt -s histappend
shopt -s histverify
shopt -s nocaseglob

# History settings
mkdir -p ~/.local/state/history
HISTFILE=~/.local/state/history/bash
HISTSIZE=10000
HISTFILESIZE=10000
HISTCONTROL="ignoredups"
HISTTIMEFORMAT=$( echo -ne "\e[0;35m[%Y-%m-%d \e[1;35m%T\e[0;35m]\e[0m " )

# Disable flow control (Ctrl-S/Q)
stty -ixon

# Start tmux or attach to existing session
if [ "$BASH_LOGIN" ] && [ -z "$TMUX" ] && [ "$UID" -ne 0 ] && [ -z "$SUDO_USER" ] && has tmux; then
  session=$( tmux list-sessions 2>/dev/null | grep -Fvm1 attached | cut -d: -f1 )
  if [ "$session" ]; then
    exec tmux attach-session -t "$session"
  else
    exec tmux new-session "$@"
  fi
fi
