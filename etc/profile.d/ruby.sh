# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# load rbenv
if [ -d ~/.rbenv ]; then
  export PATH="~/.rbenv/bin:$PATH"
  eval "$(rbenv init - --no-rehash)"

  # show ruby version in prompt
  function __rbenv_ps1 {
    local version=`rbenv version 2>/dev/null`
    version="${version/ */}"

    if [ -n "$version" -a "$version" != "system" ]; then
      version="${version/%-*/}"
      version="${version/%\.[0-9]/}"
      printf -- "[$version]"
    fi
  }

  export RBENV_PS1='\[\e[1;33m\]$(__rbenv_ps1)\[\e[0m\]'
  export PS1=$SUDO_PS1$RBENV_PS1$GIT_PS1
fi

# sudo wrapper for RubyGems
function _gem_exec {
  local command="$1"
  shift

  if rbenv version | grep -q ^system && [[ "$1" =~ (install|uninstall|update|clean|pristine) ]]; then
    command="sudo $command"
  fi

  $command "$@"
}

alias gem="_gem_exec gem"
