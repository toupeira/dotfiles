#!/bin/bash
# shellcheck disable=SC2016,SC2034,SC2154

[ "$BASH_INTERACTIVE" ] || return

# Prompt configuration
PS1_USER="𝝺"
PS1_HOST=" "
[ "$SSH_CONNECTION" ] || [ "$SUDO_USER" ] && PS1_USER="\u"
[ "$SSH_CONNECTION" ] && PS1_HOST="@\h "
[ "$EMACS" ] && PS1_USER="" && PS1_HOST=""
[ "$UID" = "0" ] && PS1_USER="\[\e[0m\e[1;31m\]$PS1_USER"

PS1="\[\e[1;35m\]\$(_prompt_jobs)\[\e[0m\]\[\e[1;30m\]$PS1_USER\[\e[1;33m\]$PS1_HOST\[\e[0;36m\][\[\e[1;36m\]\$_prompt_dir\[\e[0;36m\]]\[\e[0m\] "
PS2=" \[\e[1;36m\]»\[\e[0m\] "

# Set current directory and update title on changes

PROMPT_COMMAND=( '
  type -p _mise_hook && _mise_hook;
  [ "$PWD" != "$_last_pwd" ] && _prompt_dir=$( _prompt_dir);
  [ ${#_prompt_dir} -gt 24 ] && mux title "${_prompt_dir:0:24}…" || mux title "$_prompt_dir";
  _last_pwd="$PWD"
' )

# Prompt helpers
function _prompt_dir {
  local dir="${PWD%/}"
  local root=$( git rev-parse --show-toplevel 2>/dev/null )

  if [ "$root" ]; then
    dir="${dir#"${root%/*}"/}"
    dir=${dir/#dotfiles\/packages\/mise\/installs\/ruby\/*\/lib\/ruby\/gems\//🗃️ }
    dir=${dir/#dotfiles\/packages\/mise\/installs\//🗃️ }
    dir=${dir/#dotfiles\//📦 }
    dir=${dir/#dotfiles/📦}
    dir=${dir/#denteo\//🦷 }
    dir=${dir/#denteo/🦷}
  elif [ "$dir" = "" ]; then
    dir="/"
  else
    dir=${dir/#\/slack\//🎱 }
    dir=${dir/#\/slack/🎱}
  fi

  dir=${dir/#$HOME/\~}
  echo "$dir"
}

function _prompt_jobs {
  local jobs=$( jobs | grep -Fvc ']   Done ' )

  if [ "$jobs" -gt 0 ]; then
    printf '[%d job%s] ' "$jobs" "$( [ "$jobs" -eq 1 ] || echo -n s )"
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
    s/ ?=//;
    s/<>/⇵/;
    s/>/↑/;
    s/</↓/;
    s/\(/[/;
    s/\)/]/;
    s/\.\.\.//;
    s/\\b(main|master)\\b/🔰/;
    s/([-_[:alnum:]]{24})[-_[:alnum:]]+/\\1…/;
  "

  GIT_PS1='$(__git_ps1 "\[\e[0;32m\]❰\[\e[1;32m\]%s\[\e[0;32m\]❱\[\e[0m\] " | sed -r "$GIT_PS1_SUBSTITUTES")'
  SUDO_PS1=$PS1
  PS1=$PS1$GIT_PS1
fi
