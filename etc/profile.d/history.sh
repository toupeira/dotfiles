# shellcheck shell=bash

# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# History settings
shopt -s histappend
export HISTSIZE=10000
export HISTFILESIZE=100000
export HISTCONTROL="ignoreboth:erasedups"
