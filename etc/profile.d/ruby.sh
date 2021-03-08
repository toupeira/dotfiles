#!/bin/bash

# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# Automatically use Gemfile when present in tree
export RUBYGEMS_GEMDEPS=-

# Load asdf
if [ -d ~/.asdf ]; then
  . ~/.asdf/asdf.sh
fi

function gem {
  if [ "$1" = "cd" ]; then
    local path
    if path=$( gem open -e echo "$2" ); then
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
    command "$command" "$@"
  fi
}

alias r='rails'
alias sr='srails'
alias srails='_spring_exec rails'
alias srspec='_spring_exec rspec -f doc'
