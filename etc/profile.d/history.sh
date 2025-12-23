#!/bin/bash

[ "$BASH_INTERACTIVE" ] || return

# History settings
mkdir -p ~/.local/state/history
[ "$HISTFILE" ] && HISTFILE=~/.local/state/history/bash
HISTSIZE=50000
HISTFILESIZE=50000
HISTCONTROL="erasedups"
HISTTIMEFORMAT=$( echo -ne "\e[0;35m[%F \e[1;35m%T\e[0;35m]\e[0m " )

export SQLITE_HISTORY=~/.local/state/history/sqlite
