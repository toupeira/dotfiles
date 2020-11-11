#!/bin/bash

# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# Load asdf
if [ -d ~/.asdf ]; then
  . ~/.asdf/asdf.sh
fi

# Aliases and functions
alias rspec='bundle exec rspec -f doc'

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

# Rails/Rake wrapper
function r {
  if grep -q ' rails ([5-9]\.' Gemfile.lock &>/dev/null || [[ "$1" =~ ^(s|server|c|console|g|generate|d|destroy|r|runner|db|dbconsole|new)$ ]]; then
    rails "$@"
  else
    bundle exec rake "$@"
  fi
}

# Spring wrappers
function _spring_exec {
  local command="$1"
  shift

  if [ -x bin/spring ]; then
    spring "$command" "$@"
  else
    bundle exec "$command" "$@"
  fi
}

alias srails='_spring_exec rails'
alias srake='_spring_exec rake'
alias srspec='_spring_exec rspec -f doc'
