#!/bin/bash

[ "$BASH_INTERACTIVE" ] || return

# Automatically use Gemfile when present in tree
# export RUBYGEMS_GEMDEPS=-

function gem {
  if [ "$1" = "cd" ]; then
    local dir
    local bundle

    [ -f Gemfile ] && bundle='bundle exec'

    if [ "$2" ]; then
      dir=$( $bundle gem open -e echo "$2" )
    else
      dir=$( $bundle gem env home | cut -d: -f1 )/gems
    fi

    if [ "$dir" ]; then
      cd "$dir" || return $?
      return 0
    else
      echo "$dir"
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

alias r='rails'
