# Check for interactive bash
[ -z "$BASH_INTERACTIVE" ] && return

# Show the login message
function login_message {
  if [ -z "$BASH_INTERACTIVE" ]; then
    return
  elif [ "$PWD" = "$HOME" ]; then
    down
    which fortune >/dev/null && fortune
    echo; echo
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
      ssh-add -l | grep -q "$key" || ssh-add "$key" </dev/null
    fi
  }

  function ssh {
    __load_key
    command ssh "$@"
  }

  function git {
    [[ "$@" =~ (clone|pull|push|fetch|remote\ up|r\ up|reup) ]] && __load_key
    command git "$@"
  }
fi

# Automatically run certain RubyGems commands with sudo
function gem {
  local gem="gem"
  [[ "$@" =~ (install|uninstall) ]] && gem="sudo gem"
  command $gem "$@"
}

# Pass a file to a local GVim instance
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
