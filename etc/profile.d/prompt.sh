#!/bin/bash

[ -n "$BASH_INTERACTIVE" ] || return

# Prompt configuration
PS1_USER="λ"
PS1_HOST=" "
[ -n "$SSH_CONNECTION" ] || [ -n "$SUDO_USER" ] && PS1_USER="\u"
[ -n "$SSH_CONNECTION" ] && PS1_HOST="@\h "
[ -n "$EMACS" ] && PS1_USER="" && PS1_HOST=""
[ "$UID" = "0" ] && PS1_USER="\[\e[0m\e[1;31m\]$PS1_USER"

PS1="\[\e[1;35m\]\$(_prompt_jobs)\[\e[0m\]\[\e[1;30m\]$PS1_USER\[\e[1;33m\]$PS1_HOST\[\e[0;36m\][\[\e[1;36m\]\$_prompt_path\[\e[0;36m\]]\[\e[0m\] "
PS2=" \[\e[1;35m\]»\[\e[0m\] "

# Update title when path changes
PROMPT_COMMAND='_last_status=$?; [ "$PWD" != "$_last_pwd" ] && _prompt_path=$( _prompt_path); [ ${#_prompt_path} -gt 24 ] && mux title "${_prompt_path:0:24}…" || mux title "$_prompt_path"; _last_pwd="$PWD"'

# Prompt helpers
function _prompt_path {
  local path="$PWD"
  local root=$( git rev-parse --show-toplevel 2>/dev/null )

  if [ -n "$root" ]; then
    path="${path#${root%/*}/}"
    path=${path/#asdf\/installs\/ruby\/*\/lib\/ruby\/gems\//♦️  }
    path=${path/#asdf\/installs\/ruby\//♦️  }
    path=${path/#dotfiles\//📦 }
    path=${path/#dotfiles/📦}
  fi

  path=${path/#$HOME/\~}
  echo "$path"
}

function _prompt_jobs {
  local jobs=`jobs | grep -Fvc 'mux title'`

  if [ $jobs -gt 0 ]; then
    printf '[%d job%s] ' $jobs "`([ $jobs -eq 1 ] || echo -n s)`"
  fi
}

function _prompt_exit_status {
  [ -n "$CYGWIN" -o "$CONQUE" ] && return

  if [ -n "$_last_status" ] && [ $_last_status -gt 0 ]; then
    tput sc
    local column=$((COLUMNS-${#_last_status}-3))

    tput cup $LINES $column
    printf '\e[1;30m[\e[1;31m%s\e[1;30m]\e[0m '  $_last_status
    tput rc
  fi
}

# Use Git prompt if available
if type __git_ps1 &>/dev/null && [ -z "$VIM" ]; then
  GIT_PS1_SHOWDIRTYSTATE=1
  GIT_PS1_SHOWSTASHSTATE=1
  GIT_PS1_SHOWUNTRACKEDFILES=1
  GIT_PS1_SHOWUPSTREAM='auto git'

  GIT_PS1_SUBSTITUTES="
    s/\\*/○/;
    s/\\+/●/;
    s/%/‽/;
    s/\\\$/$/;
    s/=//;
    s/<>/ ⇵/;
    s/>/ ↑/;
    s/</ ↓/;
    s/\\b(main|master)\\b /🔰/;
    s/\\b(main|master)\\b/🔰/;
    s/([-_[:alnum:]]{24})[-_[:alnum:]]+/\\1…/;
  "

  GIT_PS1='$(__git_ps1 "\[\e[0;32m\]❰\[\e[1;32m\]%s\[\e[0;32m\]❱\[\e[0m\] " | sed -r "$GIT_PS1_SUBSTITUTES")'
  SUDO_PS1=$PS1
  PS1=$PS1$GIT_PS1
fi
