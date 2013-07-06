# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# Show the login message
function login_message {
  if [ -z "$BASH_LOGIN" ]; then
    return
  elif [ "$PWD" = "$HOME" ]; then
    if [ "$SSH_CONNECTION" -o -z "$DISPLAY" ]; then
      [ -f /etc/motd ] && cat /etc/motd
      uptime
      echo

      if [ -n "$CYGWIN" ]; then
        local hostname="$HOSTNAME"
      else
        local hostname=`hostname -f`
      fi

      echo -e "\033[0;36mWelcome \033[1;36m${USERNAME:-$USER}\033[0;36m on \033[1;33m$hostname\033[0m"
    else
      down
      has fortune && fortune && echo
    fi
    echo
    date "+%A, %e. %B %Y  %k:%M"
    echo; eval ls; echo
  else
    down
  fi
}

# Clear the screen and show the prompt at the bottom
function down {
  local i
  for i in `seq 1 $(tput lines)`; do
    echo
  done
}

# Automatically load key for SSH and Git commands
if [ -n "$SSH_AUTH_SOCK" ]; then
  function __load_key {
    local key=~/.ssh/id_rsa
    if [ -f "$key" ]; then
      ssh-add -l | fgrep -q "/.ssh/id_rsa (RSA)" || ssh-add "$key" </dev/null
    fi
  }

  function ssh {
    __load_key
    command ssh "$@"
  }

  function git {
    [[ "$1" =~ (clone|pull|push|fetch|remote\ up|up|pu|r\ up|reup) ]] && __load_key
    command git "$@"
  }
fi

# GVim wrapper to pass a file to an existing session
function gvi.add {
  gvim --remote-silent "$@"
  if has xwit; then
    xwit -raise -focus -property WM_CLASS -names gvim
  fi
}

# Ag wrapper to edit files matching a pattern (using Vim)
function ag.edit {
  [ "$1" = "-l" ] && shift
  local files=`ag -l "$@"`
  if [ -n "$files" ]; then
    sensible-vim $files
  else
    echo "No files found."
  fi
}

# Ag wrapper to search through a Gem folder inside a Rails project
function ag.rails {
  local gem="$1"
  local path=`bundle show "$gem"` || return $?
  shift

  ag "$@" "$path"
}

# GVim wrapper for SSH connections to pass a file to a local instance
if [ -n "$SSH_CONNECTION" ]; then
  unalias gvi

  function gvi {
    local wait=1

    if [ -z "$DISPLAY" -o ! -x /usr/bin/gvim ]; then
      echo "Can't find GVim instance..."
      vim "$@"
      return
    fi

    while true; do
      # Check if a GVim server is running, this will be passed through X11 to the local instance
      if command gvim --serverlist | grep -q .; then
        args=()
        options=
        for arg in "$@"; do
          if [ "${arg:0:1}" = "@" ]; then
            options="$options --servername ${arg:1}"
          else
            # Transform file paths into scp:// URIs
            args=( "${args[@]}" "scp://$HOSTNAME/`readlink -f "$arg"`" )
          fi
        done

        # Execute GVim in the root directory to avoid errors when the cwd
        # doesn't exist on the local machine
        pushd / >/dev/null
        command gvim $options --remote "${args[@]}"
        popd >/dev/null

        return
      else
        # Wait until a local GVim server can be found
        [ $wait -eq 1 ] && echo "Waiting for GVim server..."
        wait=0
        sleep 1
      fi
    done
  }
fi

# Git wrapper for project directories
function src {
  local project="$1"
  shift

  if [ -z "$project" ]; then
    echo "Usage: src PROJECT [GIT-COMMAND] [GIT-ARGS]"
    return 255
  fi

  local path=~/src/"$project"

  if [ -f "$path" ]; then
    xdg-open "$path"
  elif [ ! -d "$path" ]; then
    echo "$path does not exist"
    return 1
  elif [ -n "$1" ]; then
    pushd "$path" >/dev/null
    git "$@"
    popd >/dev/null
  else
    # switch to the project directory if no arguments were passed
    cd "$path" || return 1
  fi
}
