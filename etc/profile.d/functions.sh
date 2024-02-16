#!/bin/bash

[ -n "$BASH_INTERACTIVE" ] || return

function login_banner {
  if [ -z "$BASH_LOGIN" ]; then
    return
  elif [ $LINES -lt 25 ] || [ -n "$FLOATING_TERMINAL" ]; then
    down
  elif [ "$PWD" = "$HOME" ]; then
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
      if [ -n "$CYGWIN" ]; then
        local hostname="$HOSTNAME"
      else
        local hostname=$( hostname -f )
      fi

      echo -e "\\e[0;36mWelcome \\e[1;36m${USERNAME:-$USER}\\e[0;36m on \\e[1;33m$hostname\\e[0m"
      echo
    fi

    if has fortune && [ -z "$SSH_CONNECTION" ]; then
      fortune -acs -n $((${LINES:-10}*40)) | awk '
        /^\(.+\)$/ { print "\033[1;32mfortune" $0 "\033[0m"; next }
        /^%$/ { next }
        { print "  \033[0;32m" $0 "\033[0m" }
      '
      echo
    fi

    ls
    echo
    local mails=$( from -c 2>/dev/null | grep -v "There are 0 messages" )
    if [ -n "$mails" ]; then
      echo -e " 🎯 \\033[1;32m$mails\\033[0m"
      echo
    fi

    true # for _ps1_exit_status
  fi
}

# Clear the screen and show the prompt at the bottom
function down {
  local i

  for (( i = 0; i < LINES; i++ )); do
    echo
  done
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
  local files=$( rg -l -- "$@" )
  if [ -n "$files" ]; then
    sensible-vim "+/$1" $files
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
  local path=$( command dotfiles --path )

  if [ $# -eq 0 ]; then
    cd "$path"
  elif [ "$1" = "b" ]; then
    if [ $# -eq 1 ]; then
      cd "$path/vim/bundle"
    elif [ -d "$path/$2" ]; then
      cd "$path/$2"
    elif [ -d "$path/vim/bundle/$2" ]; then
      cd "$path/vim/bundle/$2"
    else
      cd "$( command ls -d "$path/vim/bundle/$2"* "$path/$2"* 2>/dev/null | head -1 )"
    fi
  elif [ "$1" = "dconf" ] && [ $# -eq 1 ]; then
    cd "$path/dconf"
  else
    /etc/dotfiles/bin/dotfiles "$@"
  fi
}

# Wrapper for ~/bin/src to switch project directories
function src {
  case "$1" in
    '')
      cd ~/src
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
      if [ -n "$project" ]; then
        cd "$project"
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
    __git_complete $alias _src_alias "$project_path"
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

# Go to project root
function up {
  local root=$( git rev-parse --show-toplevel 2>/dev/null )
  [ -n "$root" ] && cd "$root" || cd ..
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
  if [ -n "$1" ]; then
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
