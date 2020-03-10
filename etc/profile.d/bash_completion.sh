# shellcheck shell=bash

# Check for interactive bash
[ -n "${BASH_VERSION-}" ] && [ -n "${PS1-}" ] || return

# Check for recent enough version of bash.
[ "${BASH_VERSINFO[0]}" -ge 4 ] || return

# Check for disabled completion
shopt -q progcomp || return

if [ -z "${BASH_COMPLETION_VERSINFO-}" ]; then
  if [ -r /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -r /etc/bash_completion ]; then
    . /etc/bash_completion
  else
    return
  fi
fi

function has_completion {
  type -p "_$1" && return
  _completion_loader "$1"
  type -p "_$1"
}

# Custom completions
# complete -o bashdefault -o default -F _root_command sudo watch
complete -o bashdefault -o default -F _command psgrep pskill
complete -o bashdefault -o default -F _command start @
complete -o bashdefault -o default -F _command grc
complete -o bashdefault -o default -F _command spring

has_completion ssh && \
  complete -F _ssh -o default -o bashdefault ping telnet host nc curl
has_completion pgrep && \
  complete -F _pgrep -o default -o bashdefault psgrep pskill
has_completion systemctl && \
  complete -F _systemctl -o default -o bashdefault sctl
has_completion journalctl && \
  complete -F _journalctl -o default -o bashdefault jctl

has pgcli && has_completion psql && \
  complete -F _psql pgcli

# git completions
if has_completion git; then
  __git_complete g _git
  function _git_c { _git_checkout; }
  function _git_create_branch { _git_checkout; }

  _completion_loader git-extras
fi

if has dotfiles; then
  __git_complete dotfiles _git "$( dotfiles --path )"
  __git_complete dt _git       "$( dotfiles --path )"
fi

# Debian completions
function _packages_available {
  local cword="${COMP_WORDS[COMP_CWORD]}"

  mapfile -t COMPREPLY < <(
    compgen -W "$( apt-cache pkgnames "$cword" 2>/dev/null )"
  )
}
complete -F _packages_available pkget pkgshow

function _packages_installed {
  local cword="${COMP_WORDS[COMP_CWORD]}"

  mapfile -t COMPREPLY < <(
    compgen -W "$( dglob "$cword" )"
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
    _git
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
complete -o bashdefault -o default -F _mux mux

# kubernetes completion
if has kubectl; then
  eval "$( kubectl completion bash )"

  alias k='kubectl'
  complete -o default -F __start_kubectl k
fi

if has kubectx; then
  alias kctx='kubectx'
  alias kns='kubens'

  has_completion kubectx && complete -F _kube_contexts kctx
  has_completion kubens  && complete -F _kube_namespaces kns
fi

if has helm; then
  eval "$( helm completion bash )"
fi
