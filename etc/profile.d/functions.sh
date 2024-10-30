#!/bin/bash

[ "$BASH_INTERACTIVE" ] || return

# Go to project root
function up {
  local root=$( git rev-parse --show-superproject-working-tree 2>/dev/null )
  [ "$root" ] || root=$( git rev-parse --show-toplevel 2>/dev/null )

  if [ "$root" ]; then
    cd "$root" || return 1
  else
    cd ..
  fi
}

# Open files with xdg-open
function open {
  for file in "$@"; do
    if [ -f "$file" ]; then
      xdg-open "$file" &>/dev/null
    else
      xdg-open "$file"
    fi
  done
}

# Clear the screen and show the prompt at the bottom
function down {
  local i
  for (( i = 0; i < LINES; i++ )); do
    echo
  done
}

# Display login banner
function login_banner {
  if [ -z "$BASH_LOGIN" ] || [ "${PWD%/}" != "$HOME" ] || [ "$FLOATING_TERMINAL" ] || { [ $LINES -lt 25 ] && ! [ "$SSH_CONNECTION" ]; }; then
    down
    return
  fi

  clear

  if [ -f /run/motd.dynamic ]; then
    local motd=$( cat /run/motd.dynamic )
  elif [ -f /etc/motd ]; then
    local motd=$( cat /etc/motd )
  else
    local motd=$( uname -a )
  fi
  echo -e "\e[1;37m$motd\e[0m"
  if [ "$( echo "$motd" | wc -l )" != 1 ]; then
    echo
  fi

  uptime
  echo
  if [ "$SSH_CONNECTION" ] || [ -z "$DISPLAY" ]; then
    echo -e "\\e[0;36mWelcome \\e[1;36m${USERNAME:-$USER}\\e[0;36m on \\e[1;33m$( hostname -f )\\e[0m"
    echo
  fi

  if [ -z "$SSH_CONNECTION" ]; then
    local event=$( calendar | grep '^\w' | sort -R | head -1 )
    if [ "$event" ]; then
      echo -e "📅 \e[0;37m$event\e[0m" | sed -r -e 's/ \s+/: /'
      echo
    fi

    fortune -acs -n $((LINES*15)) \
      | fold -sw $((COLUMNS-3)) \
      | awk '
        NR == 1 && match($0, /^\(.+\)$/) { print "🍪 \033[0;32m❰\033[1;32m" substr($0, RSTART+1, RLENGTH-2) "\033[0;32m❱\033[0m"; next }
        NR == 2 { next }
        { print "  \033[0;32m" $0 "\033[0m" }
      '
    echo
  fi

  ls -C
  echo
  local mails=$( from -c 2>/dev/null | grep -v "There are 0 messages" )
  if [ "$mails" ]; then
    echo -e " 🎯 \\033[1;32m$mails\\033[0m"
    echo
  fi
}

# Move a file or directory and replace it by a symlink to the new location
function mvln {
  if [ "$1" = "-n" ]; then
    local echo="echo"
    shift
  else
    local echo=""
  fi

  local source=$( realpath "$1" 2>/dev/null )
  local target="${2%/}"

  if [ -z "$source" ] || [ -z "$target" ]; then
    echo "Usage: mvln [-n] SOURCE TARGET"
    return 1
  elif [ ! -e "$source" ]; then
    echo "$1: No such file or directory"
    return 1
  elif [ -L "$source" ]; then
    echo "$1: Is already a symlink"
    return 1
  elif [ "$source" = "$( realpath "$target" )" ]; then
    echo "$1: Source and target are the same."
    return 1
  fi

  if [ -d "$target" ]; then
    local link_target="$target/$( basename "$source" )"
  else
    local link_target="$target"
  fi

  $echo mv -v "$source" "$target" || return 1
  $echo ln -sv "$link_target" "$source"
}

# rg wrapper to edit files matching a pattern
function rg.edit {
  [ "$1" = "-l" ] && shift

  local files
  mapfile -t files < <( rg -l -- "$@" )
  if [ "${#files[@]}" ]; then
    sensible-vim "+silent /\\v$1" "+normal ggn" "${files[@]}"
  else
    echo "No files found."
  fi
}

# rg wrapper to view colored and grouped results in less
function rg.less {
  rg --pretty "$@" | less
}

# rg wrapper to search application directories
function rg.app {
  local dirs=()
  for dir in app lib ee/app ee/lib; do
    if [ -d "$dir" ]; then
      dirs=( "${dirs[@]}" "$dir" )
    fi

  done

  rg "$@" "${dirs[@]}"
}

# Switch to dotfiles repository if no arguments are passed
function dotfiles {
  local path
  local dotfiles="/slack/dotfiles"

  if [ $# -eq 0 ]; then
    cd "$dotfiles" || return 1
  elif [ "$1" = "cd" ]; then
    if [ ! "$2" ]; then
      path="$dotfiles"
    elif [ -d "$dotfiles/$2" ]; then
      path="$dotfiles/$2"
    elif path=$( dt list mise "$2" | head -1 ) && [ "$path" ]; then
      path="$dotfiles/packages/mise/installs/$path/latest"
    elif path=$( dt list lazy "$2" | head -1 ) && [ "$path" ]; then
      path="$dotfiles/packages/lazy/$path"
    fi

    if [ -d "$path" ]; then
      cd "$path" || return 1
    else
      echo "$2: No such directory or plugin"
      return 1
    fi
  else
    "$dotfiles/bin/dotfiles" "$@"
  fi
}

# Wrapper for ~/bin/src to switch project directories
function src {
  case "$1" in
    '')
      cd ~/src || return 1
      ;;
    -*|status|st|s|list|ls|l|each)
      command src "$@"
      ;;
    *)
      if [ $# -gt 1 ]; then
        command src "$@"
        return
      fi

      local project=$( command src "$1" --path )
      if [ "$project" ]; then
        cd "$project" || return 1
      fi
      ;;
  esac
}

# Helper to create an alias for src with Git completion
function src_alias {
  local alias="$1"
  local project="$2"
  shift 2

  if [ -d "$project" ]; then
    local project_path="$project"
  else
    local project_path="$( command src --path )/$project"
  fi

  if [ -d "$project_path" ]; then
    local space=''
    [ $# -gt 0 ] && space=' '
    alias $alias="src $project$space$*"
    __git_complete "$alias" _src_alias
  else
    return 1
  fi
}

# Sudo wrapper for systemctl
function systemctl {
  local command="systemctl"
  local args=( "$@" )

  while [ "${1:0:1}" = "-" ]; do
    [[ "$1" =~ ^(-h|--help|--version)$ ]] && shift $#
    shift
  done

  if ! [[ "$1" =~ ^(|status|(get|is|list|show).*)$ ]] && ! [[ "${args[*]}" =~ --user ]]; then
    command="sudo systemctl"
  fi

  command $command "${args[@]}"
}

# Open multiple SSH sessions in tmux panes
function ssh.mux {
  if [ $# -eq 0 ]; then
    echo "Usage: ssh.mux HOST.."
    return 1
  fi

  local first_host="$1"
  shift

  for host in $( echo "$@" | tr " " "\\n" | tac ); do
    tmux split-window -vd ssh "$host"
  done

  tmux select-layout even-vertical
  ssh "$first_host"
}

# Browse a JSON file
function jq.less {
  jq -C . "$1" | less
}

# Tail a JSON logfile
function jq.tail {
  tail -f "$1" | jq --unbuffered .
}

# Serve a directory over HTTP
function serve {
  if [ "$1" ]; then
    local port="$1"
  else
    local port=$((9000 + RANDOM % 1000))
  fi

  xdg-open "http://localhost:$port/" &>/dev/null
  python3 -m http.server "$port"
}

function dusort {
  (
    if [ $# -eq 0 ]; then
      du -sch -- *
    elif [ $# -eq 1 ]; then
      du -sch -- "${1%/}"/*
    else
      du -sch -- "${@%/}"
    fi
  ) | sort -h
}

function sman  {
  if [ $# -eq 0 ]; then
    cd /slack/documents/Manuals || return
  else
    command sman "$@"
  fi
}

function sheet {
  if [ $# -eq 0 ]; then
    cd /slack/documents/Noten || return
  else
    command sheet "$@"
  fi
}

function mise.search {
  mise plugins ls-remote | grep "$1"
}

function mise.add {
  mise use -g --pin "$1@latest"
}
