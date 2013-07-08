# Check for interactive bash and that we haven't already been sourced.
[ -n "$BASH_INTERACTIVE" -a -z "$BASH_COMPLETION" -a -z "$BASH_COMPLETION_COMPAT_DIR" ] || return

# Check for recent enough version of bash.
bash=${BASH_VERSION%.*}; bmajor=${bash%.*}; bminor=${bash#*.}
if [ $bmajor -gt 4 ] || [ $bmajor -eq 4 -a $bminor -ge 1 ]; then
    [ -r "${XDG_CONFIG_HOME:-$HOME/.config}/bash_completion" ] && \
        . "${XDG_CONFIG_HOME:-$HOME/.config}/bash_completion"
    if shopt -q progcomp && [ -r /etc/bash_completion ]; then
        # Source completion code.
        . /etc/bash_completion
    fi
fi
unset bash bmajor bminor

# Custom completions
complete -o bashdefault -o default -F _root_command sudo watch
complete -o bashdefault -o default -F _command start
complete -F _pgrep psgrep
complete -F _gem18 gem

# Debian completions
function _packages_available {
  COMPREPLY=( $(compgen -W "`apt-cache pkgnames ${COMP_WORDS[COMP_CWORD]} 2>/dev/null`") )
}
complete -F _packages_available pkget

function _packages_installed {
  COMPREPLY=( $(compgen -W "`dglob ${COMP_WORDS[COMP_CWORD]}`") )
}
complete -F _packages_installed pkglist pkgpurge pkgremove debbugs debpackages

# Git completions
. /usr/share/bash-completion/completions/git

function __git_complete_in_workdir {
	local wrapper="__git_wrap_in_workdir_${1}_${2}"
  local function

	read -r -d '' function <<-EOF
    function $wrapper {
      __GIT_COMPLETE_WORKDIR="$3";
      __GIT_COMPLETE_DEFAULT=1;

      __git_func_wrap $2;
      if [ -z "\$COMPREPLY" -a -n "\$__GIT_COMPLETE_DEFAULT" ]; then
        COMPREPLY=( \$(cd "$3"; compgen -o bashdefault -o default -o nospace -- "\${COMP_WORDS[COMP_CWORD]}") );
      fi;

      unset __GIT_COMPLETE_DEFAULT;
    }
EOF
  eval $function

	complete -o nospace -F $wrapper $1 2>/dev/null \
		|| complete -o nospace -F $wrapper $1
}

__git_complete_in_workdir dotfiles _git `dotfiles path`
__git_complete_in_workdir dt _git       `dotfiles path`

function _src_projects {
  COMPREPLY=( $(compgen -W "`find ~/src -mindepth 1 -maxdepth 1 -name "${COMP_WORDS[COMP_CWORD]}*" -printf '%f '`") )
}
complete -F _src_projects src

function _git_edit {
  # disable default completion for __git_complete
  compopt +o bashdefault +o default
  # disable default completion for __git_complete_in_workdir
  unset __GIT_COMPLETE_DEFAULT

  COMPREPLY=( $([ -n "$__GIT_COMPLETE_WORKDIR" ] && cd "$__GIT_COMPLETE_WORKDIR"; git-edit --complete $cur) )
}

function _ed {
  cur=${COMP_WORDS[COMP_CWORD]}
  _git_edit
}

complete -o nospace -F _ed ed
