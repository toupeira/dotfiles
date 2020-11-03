#!/bin/bash

# Helper to check for commands
has() {
  which "$1" &>/dev/null
}

# Define helper variables for interactive and login shells
[ -n "$BASH_VERSION" ] && [ -n "$PS1" ] && BASH_INTERACTIVE=1
[ -n "$BASH_INTERACTIVE" ] && shopt -q login_shell && BASH_LOGIN=1

# Only continue in login shells
[ -n "$BASH_LOGIN" ] || return

# Tweak globbing
shopt -s extglob
shopt -s globstar

# Disable flow control (Ctrl-S/Q)
stty -ixon

if [ -z "$TMUX" ] && [ -z "$SUDO_USER" ] && type tmux &>/dev/null; then
  # ignore stale tmux sockets
  if [ -n "$TMUX" ] && [ ! -S "$TMUX" ]; then
    unset TMUX
  fi

  session=$( tmux list-sessions 2>/dev/null | grep -vm1 attached | cut -d: -f1 )
  if [ -n "$session" ] && [ $# -eq 0 ]; then
    exec tmux attach-session -t "$session"
  else
    exec tmux new-session "$@"
  fi
fi
