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
shopt -s extglob
shopt -s globstar
shopt -s nocaseglob
shopt -s checkwinsize

# Disable flow control (Ctrl-S/Q)
stty -ixon

# Start tmux or attach to existing session
if [ "$BASH_LOGIN" ] && [ -z "$TMUX" ] && [ "$UID" -ne 0 ] && [ -z "$SUDO_USER" ] && has tmux; then
  session=$( tmux list-sessions 2>/dev/null | grep -Fvm1 attached | cut -d: -f1 )
  if [ "$session" ] && [ $# -eq 0 ]; then
    exec tmux attach-session -t "$session"
  else
    exec tmux new-session "$@"
  fi
fi
