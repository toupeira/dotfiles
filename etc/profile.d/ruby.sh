# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# load rbenv
if has rbenv; then
  eval "$(rbenv init - --no-rehash)"

  # show ruby version in prompt
  function __rbenv_ps1 {
    local version=`rbenv version`
    version="${version/ */}"

    if [ "$version" != "system" ]; then
      version="${version/%-*/}"
      version="${version/%\.0/}"
      printf -- "[$version]"
    fi
  }

  export RBENV_PS1='\[\e[1;35m\]$(__rbenv_ps1)\[\e[0m\]'
  export PS1=$SUDO_PS1$RBENV_PS1$GIT_PS1
fi

# sudo wrapper for RubyGems
function _gem_exec {
  local command="$1"
  shift

  if rbenv version | grep -q ^system && [[ "$1" =~ (install|uninstall|update|clean|pristine) ]]; then
    command="sudo $command"
  fi

  echo $command "$@"
}

alias gem="_gem_exec gem"
alias sugem="sudo gem"
