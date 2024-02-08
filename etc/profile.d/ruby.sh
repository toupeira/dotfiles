#!/bin/bash

[ -n "$BASH_INTERACTIVE" ] || return

# Automatically use Gemfile when present in tree
# export RUBYGEMS_GEMDEPS=-

function gem {
  if [ "$1" = "cd" ]; then
    local path
    if [ -n "$2" ]; then
      path=$( bundle exec gem open -e echo "$2" )
    else
      path=$( bundle exec gem env path | cut -d: -f1 )/gems
    fi

    if [ -n "$path" ]; then
      cd "$path" || return $?
      return 0
    else
      echo "$path"
      return 1
    fi
  fi

  command gem "$@"
}

function _bundle_exec {
  local command="$1"
  shift

  if [ -f Gemfile ]; then
    bundle exec "$command" "$@"
  else
    command "$command" "$@"
  fi
}

alias be='bundle exec '
alias rake='_bundle_exec rake'
alias rspec='_bundle_exec rspec'
alias rubocop='_bundle_exec rubocop'

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
