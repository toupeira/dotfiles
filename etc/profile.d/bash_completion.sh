#!/bin/bash

[ "$BASH_INTERACTIVE" ] || return

export GIT_COMPLETION_SHOW_ALL=1
export GIT_COMPLETION_SHOW_ALL_COMMANDS=1

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

has_completion pgrep _comp_cmd_pgrep && complete -F _comp_cmd_pgrep psgrep pskill
has_completion systemctl && complete -F _systemctl sctl
has_completion journalctl && complete -F _journalctl jctl

# git completions
if has_completion git __git_main; then
  __git_complete g git
  function _git_create_branch { _git_checkout; }
  function _git_delete_branch { _git_checkout; }

  if has dotfiles; then
    __git_complete dotfiles git
    __git_complete dt git
  fi
fi

# Debian completions
function _packages_available {
  local cword="${COMP_WORDS[COMP_CWORD]}"
  mapfile -t COMPREPLY < <(
    compgen -W "$( apt-cache --no-generate pkgnames -- "$cword" )"
  )
}
complete -F _packages_available pkget pkgshow

function _packages_installed {
  local cword="${COMP_WORDS[COMP_CWORD]}"
  mapfile -t COMPREPLY < <(
    _xfunc dpkg _comp_dpkg_installed_packages "$cword"
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
    __git_wrap__git_main
  fi
}

# mux completion
function _mux {
  local cword="${COMP_WORDS[COMP_CWORD]}"

  if [ "${cword:0:1}" = "@" ]; then
    local commands=( bundle console dev log migrate server watcher )
    mapfile -t COMPREPLY < <(
      compgen -W "$(
        (printf '%s:\n' "${commands[@]}"; cat Procfile 2>/dev/null) \
          | grep -Eo "^${cword:1}[-[:alnum:]]*" \
          | sed -r 's/^/@/'
      )"
    )
  else
    _command
  fi
}
complete -F _mux mux

# notes completion
function _notes {
  local cword="${COMP_WORDS[COMP_CWORD]}"

  mapfile -t COMPREPLY < <(
    compgen -W "$( fd . /slack/scrapbook/notes -t f -x echo '{/.}' )" "$cword"
  )
}

complete -F _notes notes n
