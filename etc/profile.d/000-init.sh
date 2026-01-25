#!/bin/bash
# shellcheck disable=SC2034

[ "$BASH_VERSION" ] && [[ "$-" == *i* ]] && BASH_INTERACTIVE=1
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
shopt -s progcomp_alias

# Disable flow control (Ctrl-S/Q)
[ -t 1 ] && stty -ixon

# Start tmux or attach to existing session
if [ "$BASH_LOGIN" ] && [ -z "$TMUX" ] && [ "$UID" -ne 0 ] && [ -z "$SUDO_USER" ] && [ -d ~/.config/tmux ]; then
  session=$( tmux list-sessions 2>/dev/null | grep -Fvm1 attached | cut -d: -f1 )
  if [ "$session" ]; then
    exec tmux attach-session -t "$session"
  else
    exec tmux new-session "$@"
  fi
fi

# Change to original directory when opening new panes in symlinked paths
if [ "$BASH_LOGIN" ] && [ "$TMUX" ]; then
  path=${PWD#/mnt}
  [ "$(realpath "$path" )" = "$PWD" ] && cd "$path"
  unset path
fi
