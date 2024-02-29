#!/bin/bash

[ "$BASH_INTERACTIVE" ] || return

# History settings
mkdir -p ~/.local/share/bash
HISTFILE=~/.local/share/bash/history
HISTSIZE=10000
HISTFILESIZE=100000
HISTCONTROL="ignoreboth:erasedups"
shopt -s histappend
