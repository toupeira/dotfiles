#!/bin/bash

[ "$BASH_LOGIN" ] || return

if [ -z "$TMUX" ] && [ -z "$SUDO_USER" ] && [ "$TERM" != "linux" ] && type tmux &>/dev/null; then
  # ignore stale tmux sockets
  if [ "$TMUX" ] && [ ! -S "$TMUX" ]; then
    unset TMUX
  fi

  session=$( tmux list-sessions 2>/dev/null | grep -vm1 attached | cut -d: -f1 )
  if [ "$session" ] && [ $# -eq 0 ]; then
    exec tmux attach-session -t "$session"
  else
    exec tmux new-session "$@"
  fi
fi
