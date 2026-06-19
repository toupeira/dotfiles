#!/bin/bash

[ "$BASH_INTERACTIVE" ] || return

export GIT_COMPLETION_SHOW_ALL=1
export GIT_COMPLETION_SHOW_ALL_COMMANDS=1

. /usr/share/bash-completion/bash_completion

function has_completion {
  local name="${2:-_$1}"

  type -p "$name" && return
  _comp_load "$1"
}

# Custom completions
complete -F _command start @
complete -F _command spring
complete -F _command pw-jack

has_completion pgrep _comp_cmd_pgrep && complete -F _comp_cmd_pgrep psgrep pskill
has_completion systemctl && complete -F _systemctl sctl sctl.user
has_completion journalctl && complete -F _journalctl jctl jctl.user

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
  _comp_compgen -c "$2" -x apt-cache packages
}
complete -F _packages_available pkget pkgshow

function _packages_installed {
  _comp_compgen -c "$2" -x dpkg purgeable_packages
}
complete -F _packages_installed pkglist pkgpurge pkgremove

# src completion
complete -C 'src list -a' src

function _src_alias {
  local cur="${COMP_WORDS[COMP_CWORD]}"

  if [ "${cur:0:1}" = "@" ]; then
    _mux
  else
    __git_wrap__git_main
  fi
}

# mux completion
function _mux {
  local cur="${COMP_WORDS[COMP_CWORD]}"

  if [ "${cur:0:1}" = "@" ]; then
    local commands=( bundle console dev edit log migrate run server test )
    mapfile -t COMPREPLY < <(
      compgen -W "$(
        (printf '%s:\n' "${commands[@]}"; cat Procfile 2>/dev/null) \
          | grep -Eo "^${cur:1}[-[:alnum:]]*" \
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
  mapfile -t COMPREPLY < <(
    obsidian --list "$2" | xargs -d $'\n' printf '%q\n'
  )
}

complete -F _notes notes n
