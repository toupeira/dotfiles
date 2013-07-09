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
    sensible-editor $files
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

# Switch project directories and run Git commands in them
function src {
  if [ "$1" = "each" ]; then
    shift
    for dir in ~/src/*; do
      echo -e "# \e[0;36m$dir\e[0m" | sed -r "s|$HOME|~|"
      exec 3>&1
      `cd "$dir"; $@ 1>&3`

      local status=$?
      if [ $status -ne 0 ]; then
        echo -e "# \e[1;31mexit code $status\e[0m"
      fi
      echo
    done

    return
  elif [[ "$1" =~ ^(|st|status)$ ]]; then
    echo
    local first=1
    local last=0

    for dir in ~/src/*; do
      [ -d "$dir/.git" ] || continue

      local changes=`cd "$dir"; git status -s | grep -c .`
      local unmerged=`cd "$dir"; git status | grep -v "^# On branch .*" | fgrep -vx "nothing to commit, working directory clean"`

      if [ $changes -gt 0 -o -n "$unmerged" ]; then
        local label="changes"

        if [ $changes -gt 0 ]; then
          local label="changes"
          [ $changes -eq 1 ] && label="change"
          label="$changes\e[1;37m $label"
        else
          label="Unmerged changes"
        fi

        [ -z "$first" ] && echo
        echo -e " \e[1;32m>\e[1;37m \e[1;33m$label in \e[1;36m[`realpath "$dir"`]\e[0m" | sed -r "s|$HOME|~|"
        (cd "$dir"; git -c color.ui=always status | sed -r 's/^/    /')
      else
        [ $last -gt 0 ] && echo
        echo -e " \e[0;32m>\e[0m No changes in \e[0;36m[`realpath "$dir"`]\e[0m" | sed -r "s|$HOME|~|"
      fi

      unset first
      last=$changes
      [ -n "$unmerged" ] && let last++
    done
    echo

    return
  fi

  local project="$1"
  local path=~/src/"$project"
  shift

  if [ -z "$project" ]; then
    echo "Usage: src PROJECT [GIT-COMMAND] [GIT-ARGS]"
    echo "       src status"
    return 255
  fi

  if [ ! -e "$path" ]; then
    echo "$path does not exist"
    return 1
  elif [ "$1" = "--path" ]; then
    echo "$path"
  elif [ -f "$path" ]; then
    sensible-editor "$path"
  elif [ ! -d "$path" ]; then
    echo "Unsupported path $path"
  elif [ -n "$1" ]; then
    pushd "$path" >/dev/null
    git "$@"
    popd >/dev/null
  else
    # switch to the project directory if no arguments were passed
    cd "$path" || return 1
  fi
}
alias st='src status'

# Helper to create an alias for src with Git completion
function src_alias {
  local alias="$1"
  local project="$2"

  alias $alias="src $project"
  __git_edit_complete $alias _git `src "$project" --path`
}
