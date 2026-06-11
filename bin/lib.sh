#!/bin/bash

STATUS=
QUIET=
VERBOSE=
DEBUG=
ICON="●"

# Helpers to set options
function set-quiet   { QUIET=1;   }
function set-verbose { VERBOSE=1; }
function set-debug   { DEBUG=1;   }
function set-icon    { ICON="$1"; }

# Check for existing commands
function has {
  command -v "$1" >/dev/null
}

# Output a message with a colored icon in front
function msg {
  local message=${1/$HOME/\~}
  local color=${2:-2}
  local icon=${3:-${ICON}}

  if [ $# -gt 3 ]; then
    shift 3
    local echo_options=( "$@" )
  fi

  if [ "$message" ]; then
    echo -e "${echo_options[@]}" " \e[1;3${color}m$icon \e[22m$message\e[0m" \
      | sed -re 's/\{\{/\o033[1m/g' \
             -e 's/\}\}/\o033[22m/g' >&2
  else
    echo -e "${echo_options[@]}" >&2
  fi
}

# Helpers for colored messages
function status  {
  local message=$(echo "$1" \
    | sed -re 's/\{\{/\o033[1;32m/g' \
           -e 's/\}\}/\o033[1;37m/g' )
  msg "\e[1;37m$message" 2;
}

function info    { [ "$QUIET" = 1 ] || msg "$1" 4; }
function warning { msg "$1" 3; }
function error   { msg "$1" 1; STATUS=1; }

# Output error message and exit
# shellcheck disable=SC2120
function abort {
  [ "$1" ] && error "$@"
  exit 1
}

# Exit with the last or configured status code
function finish {
  exit "${STATUS:-$?}"
}

# Ask a yes/no question, defaulting to no
function ask {
  msg "\e[0m$1 \e[0;36m[y/{{N}}] " 6 "" -n
  read -r
  [ "${REPLY:0:1}" = "Y" ] || [ "${REPLY:0:1}" = "y" ]
}

# Ask a yes/no question, defaulting to yes
function ask-yes {
  msg "\e[0m$1 \e[0;36m[{{Y}}/n] " 6 "" -n
  read -r
  [ "${REPLY:0:1}" != "N" ] && [ "${REPLY:0:1}" != "n" ]
}

# Ask to continue or abort
function ask-abort {
  [ "$1" ] && error "$@"
  if ! ask "{{Do you want to continue?}}" 1; then
    abort
  fi
}

# Output verbose message
function verbose {
  [ "$VERBOSE" = 1 ] || return
  info "$@"
}

# Output debug message
function debug {
  [ "$DEBUG" = 1 ] || return
  echo -e "\e[0;35m$( printf '%10s' "$1" ): │ \e[1m$2\e[0m" >&2
}

# Output indentation
function indent {
  local count=${1:-2}
  for (( i = 0; i < count; i++ )) do
    printf ' '
  done
}
