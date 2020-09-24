# shellcheck shell=bash

# Add rbenv binstubs
if [ -d ~/.rbenv ]; then
  export PATH="$PATH:$HOME/.rbenv/bin"
fi

# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# Load rbenv
if [ -d ~/.rbenv ]; then
  eval "$(rbenv init - --no-rehash)"
fi

# sudo wrapper for RubyGems
function _gem_exec {
  local command="$1"
  shift

  if rbenv version | grep -q ^system && [[ "$1" =~ (install|uninstall|update|clean|pristine) ]]; then
    command="sudo $command"
  elif [ "$1" = "cd" ]; then
    local path
    path=$( bundle exec gem open -e echo "$2" )
    if [ $? -eq 0 ]; then
      cd "$path"
      return
    else
      echo "$path"
      return 1
    fi
  fi

  $command "$@"
}

# Automatically use rails/rake commands
function r {
  if grep -q ' rails ([5-9]\.' Gemfile.lock &>/dev/null || [[ "$1" =~ ^(s|server|c|console|g|generate|d|destroy|r|runner|db|dbconsole|new)$ ]]; then
    rails "$@"
  else
    rake "$@"
  fi
}

# Automatically use spring wrappers
function _spring_exec {
  local command="$1"
  shift

  if [ -x bin/spring ]; then
    spring "$command" "$@"
  else
    command "$command" "$@"
  fi
}

alias gem='_gem_exec gem'
alias sugem='sudo /usr/bin/gem'

alias rspec='rspec -f doc'

alias _spring_exec='_spring_exec '
alias srails='_spring_exec rails'
alias srake='_spring_exec rake'
alias srspec='_spring_exec rspec'

# gem aliases
has rubocop && alias rubocop='rubocop -D'
