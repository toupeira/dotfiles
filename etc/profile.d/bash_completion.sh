# Check for interactive bash and that we haven't already been sourced.
[ -n "$BASH_INTERACTIVE" -a -z "$BASH_COMPLETION" -a -z "$BASH_COMPLETION_COMPAT_DIR" ] || return

# Check for recent enough version of bash.
[ ${BASH_VERSINFO[0]} -ge 4 -a ${BASH_VERSINFO[1]} -ge 1 ] || return

# Check for disabled completion
shopt -q progcomp || return

if [ "`type -t _git`" != "function" -a -r /usr/share/bash-completion/completions/git ]; then
  . /usr/share/bash-completion/completions/git
fi

if [ -r /usr/share/bash-completion/bash_completion ]; then
  . /usr/share/bash-completion/bash_completion
elif [ -r /etc/bash_completion ]; then
  . /etc/bash_completion
else
  return
fi

# Custom completions
complete -o bashdefault -o default -F _root_command sudo watch
complete -o bashdefault -o default -F _command start @

if has dotfiles; then
  __git_edit_complete dotfiles _git `dotfiles --path`
  __git_edit_complete dt _git       `dotfiles --path`
fi

# Debian completions
function _packages_available {
  COMPREPLY=( $(compgen -W "`apt-cache pkgnames ${COMP_WORDS[COMP_CWORD]} 2>/dev/null`") )
}
complete -F _packages_available pkget

function _packages_installed {
  COMPREPLY=( $(compgen -W "`dglob ${COMP_WORDS[COMP_CWORD]}`") )
}
complete -F _packages_installed pkglist pkgpurge pkgremove debbugs debpackages

function _src_projects {
  local src_dir=~/src
  COMPREPLY=( $(compgen -W "`src list -a | grep "${COMP_WORDS[COMP_CWORD]}"`") )
}
complete -F _src_projects src
