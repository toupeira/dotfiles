#!/bin/bash

# shellcheck shell=sh disable=SC1091,SC2039,SC2166
# Check for interactive bash and that we haven't already been sourced.
if [ "x${BASH_VERSION-}" != x -a "x${PS1-}" != x -a "x${BASH_COMPLETION_VERSINFO-}" = x ]; then

    # Check for recent enough version of bash.
    if [ "${BASH_VERSINFO[0]}" -gt 4 ] ||
        [ "${BASH_VERSINFO[0]}" -eq 4 -a "${BASH_VERSINFO[1]}" -ge 2 ]; then
        [ -r "${XDG_CONFIG_HOME:-$HOME/.config}/bash_completion" ] &&
            . "${XDG_CONFIG_HOME:-$HOME/.config}/bash_completion"
        if shopt -q progcomp && [ -r /usr/share/bash-completion/bash_completion ]; then
            # Source completion code.
            . /usr/share/bash-completion/bash_completion
        fi
    fi
fi

[ -n "$BASH_INTERACTIVE" ] || return

function has_completion {
  local name="${2:-_$1}"

  type -p "$name" && return
  _completion_loader "$1"
  type -p "$name"
}

# Custom completions
complete -F _command psgrep pskill
complete -F _command start @
complete -F _command spring
complete -F _command pw-jack

has_completion pgrep && complete -F _pgrep psgrep pskill
has_completion systemctl && complete -F _systemctl sctl
has_completion journalctl && complete -F _journalctl jctl

# git completions
if has_completion git __git_main; then
  __git_complete g __git_main
  function _git_c { _git_checkout; }
  function _git_create_branch { _git_checkout; }
fi

if has dotfiles; then
  _path=$( dotfiles --path )
  __git_complete dotfiles __git_main "$_path"
  __git_complete dt __git_main       "$_path"
  unset _path
fi

# Debian completions
function _packages_available {
  local cword="${COMP_WORDS[COMP_CWORD]}"

  mapfile -t COMPREPLY < <(
    compgen -W "$( apt-cache --no-generate pkgnames -- "$cword" 2>/dev/null )"
  )
}
complete -F _packages_available pkget pkgshow

function _packages_installed {
  local cword="${COMP_WORDS[COMP_CWORD]}"

  mapfile -t COMPREPLY < <(
    compgen -W "$( dglob -- "$cword" 2>/dev/null )"
  )
}
complete -F _packages_installed pkglist pkgpurge pkgremove debbugs debpackages

# src completion
function _src_projects {
  local cword="${COMP_WORDS[COMP_CWORD]}"

  mapfile -t COMPREPLY < <(
    compgen -W "$( src list -a "$cword" )"
  )
}
complete -F _src_projects src

function _src_alias {
  local cword="${COMP_WORDS[COMP_CWORD]}"
  if [ -n "$cword" -a "${cword:0:1}" = "@" ]; then
    _mux
  else
    __git_main
  fi
}

# mux completion
function _mux {
  local cword="${COMP_WORDS[COMP_CWORD]}"

  if [ -n "$cword" -a "${cword:0:1}" = "@" ]; then
    [ "$cword" = "@" ] && cword="@\w"

    mapfile -t COMPREPLY < <(
      compgen -W "$(
        (echo -e "bundle:\nconsole:\nlog:\nmigrate:\nserver:\nwatcher:"; cat Procfile 2>/dev/null) \
          | egrep -o "^${cword:1}[-[:alnum:]]*" \
          | sed -r 's/^/@/'
      )"
    )
  else
    _command
  fi
}
complete -F _mux mux
