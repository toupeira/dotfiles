#!/bin/bash

[ "$BASH_INTERACTIVE" ] || return

# History settings
mkdir -p ~/.local/state/history
HISTFILE=~/.local/state/history/bash
HISTSIZE=50000
HISTFILESIZE=50000
HISTCONTROL="ignoredups"
HISTTIMEFORMAT=$( echo -ne "\e[0;35m[%F \e[1;35m%T\e[0;35m]\e[0m " )

export SQLITE_HISTORY=~/.local/state/history/sqlite

# TODO: remove
if [ -f ~/.zsh_history ]; then
  mv ~/.zsh_history ~/.local/state/history/zsh
fi

if [ -f ~/.psql_history ]; then
  mv ~/.psql_history ~/.local/state/history/psql
fi

if [ -f ~/.sqlite_history ]; then
  mv ~/.sqlite_history ~/.local/state/history/sqlite
fi

if [ -f ~/.local/share/pry/pry_history ]; then
  mv ~/.local/share/pry/pry_history ~/.local/state/history/pry
  rmdir ~/.local/share/pry
fi

if [ -f ~/.local/state/history/fzf-shell-history ]; then
  mv ~/.local/state/history/fzf-shell-history ~/.local/state/history/fzf-history
fi
