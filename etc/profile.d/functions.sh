# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# Show the login message
function login_message {
  if [ -z "$BASH_LOGIN" ]; then
    return
  elif [ "$PWD" = "$HOME" ]; then
    if [ "$SSH_CONNECTION" -o -z "$DISPLAY" ]; then
      [ -n "$STY" ] && cat /etc/motd
      uptime
      echo
      echo -e "\033[0;36mWelcome \033[1;36m$USER\033[0;36m on \033[1;33m`hostname -f`\033[0m"
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

# Vim wrapper to open files matching a pattern (using ack)
function ack.edit {
  [ "$1" = "-l" ] && shift
  local files=`ack -l "$@"`
  if [ -n "$files" ]; then
    if [ -n "$DISPLAY" -a -z "$SSH_CONNECTION" ]; then
      gvim $files
    else
      vim $files
    fi
  else
    echo "No files found."
  fi
}

# GVim wrapper to pass a file to a local instance
if [ -n "$SSH_CONNECTION" ]; then
  unalias gvi
  function gvi {
    local wait=1

    while true; do
      [ -n "$DISPLAY" -a -x /usr/bin/gvim ] || break

      if gvim --serverlist | grep -q .; then
        args=()
        options=
        for arg in "$@"; do
          if [ "${arg:0:1}" = "@" ]; then
            options="$options --servername ${arg:1}"
          else
            args=( "${args[@]}" "scp://$HOSTNAME/`readlink -f "$arg"`" )
          fi
        done
        pushd / >/dev/null
        gvim $options --remote "${args[@]}"
        popd >/dev/null
        return
      else
        [ $wait -eq 1 ] && echo "Waiting for GVim server..."
        wait=0
        sleep 1
      fi
    done

    vim "$@"
  }
fi

# Mount a loopback device
function mount.loop {
  mount="/mnt/loop"
  [ -n "$2" ] && mount="$2"

  if [ ! -n "$1" ]; then
    echo "Usage: mount.loop IMAGE [MOUNTPOINT]"
    return 255
  elif [ ! -f "$1" ]; then
    echo "Image $1 not found."
    return 1
  elif mountpoint -q "$mount"; then
    echo "$mount is already mounted."
    return 1
  else
    sudo mount -v -o loop "$1" "$mount" | tail -1
  fi
}
