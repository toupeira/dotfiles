#!/bin/bash

[ "$BASH_INTERACTIVE" ] || return

# History settings
mkdir -p ~/.local/state/history
HISTFILE=~/.local/state/history/bash
HISTSIZE=10000
HISTFILESIZE=10000
HISTCONTROL="ignoreboth:erasedups"
shopt -s histappend

# TODO: remove
if [ -f ~/.local/share/bash/history ]; then
  mv ~/.local/share/bash/history ~/.local/state/history/bash
  rmdir ~/.local/share/bash
fi
