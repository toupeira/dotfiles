#!/bin/bash

[ -n "$BASH_INTERACTIVE" ] || return

. /usr/share/bash-completion/bash_completion

function has_completion {
  local name="${2:-_$1}"

  type -p "$name" && return
  _completion_loader "$1"
  type -p "$name"
}

# Custom completions
complete -F _command start @
complete -F _command spring
complete -F _command pw-jack

has_completion pgrep && complete -F _pgrep psgrep pskill
has_completion systemctl && complete -F _systemctl sctl
has_completion journalctl && complete -F _journalctl jctl

# git completions
if has_completion git __git_main; then
  __git_complete g __git_main
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
  local cur="${COMP_WORDS[COMP_CWORD]}"
  mapfile -t COMPREPLY < <(
    _xfunc apt-cache _apt_cache_packages
  )
}
complete -F _packages_available pkget pkgshow

function _packages_installed {
  local cur="${COMP_WORDS[COMP_CWORD]}"
  mapfile -t COMPREPLY < <(
    _xfunc dpkg _comp_dpkg_installed_packages "$cur"
  )
}
complete -F _packages_installed pkglist pkgpurge pkgremove

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
  if [ "${cword:0:1}" = "@" ]; then
    _mux
  else
    __git_main
  fi
}

# mux completion
function _mux {
  local cword="${COMP_WORDS[COMP_CWORD]}"

  if [ "${cword:0:1}" = "@" ]; then
    [ "$cword" = "@" ] && cword="@\w"

    mapfile -t COMPREPLY < <(
      compgen -W "$(
        (echo -e "bundle:\nconsole:\ndev:\nlog:\nmigrate:\nserver:\nwatcher:"; cat Procfile 2>/dev/null) \
          | grep -Eo "^${cword:1}[-[:alnum:]]*" \
          | sed -r 's/^/@/'
      )"
    )
  else
    _command
  fi
}
complete -F _mux mux
