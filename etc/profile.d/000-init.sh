#!/bin/bash
# shellcheck disable=SC2034

[ "$BASH_VERSION" ] && [ "$PS1" ] && BASH_INTERACTIVE=1
[ "$BASH_INTERACTIVE" ] && shopt -q login_shell && BASH_LOGIN=1

[ "$BASH_INTERACTIVE" ] || return

# Helper to check for commands
has() {
  command -v "$1" &>/dev/null
}

# Tweak globbing
shopt -s extglob
shopt -s globstar
shopt -s nocaseglob

# Disable flow control (Ctrl-S/Q)
stty -ixon
