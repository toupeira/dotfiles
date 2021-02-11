#!/bin/bash

# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# Load asdf
if [ -d ~/.asdf ]; then
  . ~/.asdf/asdf.sh
fi

function gem {
  if [ "$1" = "cd" ]; then
    local path
    if path=$( bundle exec gem open -e echo "$2" ); then
      cd "$path" || return $?
      return 0
    else
      echo "$path"
      return 1
    fi
  fi

  command gem "$@"
}

function _spring_exec {
  local command="$1"
  shift

  if [ -x bin/spring ]; then
    spring "$command" "$@"
  else
    bundle exec "$command" "$@"
  fi
}

alias be='bundle exec'
alias rspec='bundle exec rspec -f doc'
alias rubocop='bundle exec rubocop'

alias r='rails'
alias sr='srails'
alias srails='_spring_exec rails'
alias srspec='_spring_exec rspec -f doc'
