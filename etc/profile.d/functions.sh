# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# Repeat a string
function _repeat {
  local string="$1"
  local count="$2"

  for (( i = 0; i < count; i++)); do
    echo -n "$string"
  done
}

# Show the login message
function login_message {
  if [ -z "$BASH_LOGIN" ]; then
    return
  elif [ "$PWD" = "$HOME" ]; then
    echo -e "\e[1;37m`uname -a`\e[0m"
    uptime
    echo

    if [ -f /etc/motd ]; then
      cat /etc/motd
      echo
    fi

    if [ "$SSH_CONNECTION" -o -z "$DISPLAY" ]; then
      if [ -n "$CYGWIN" ]; then
        local hostname="$HOSTNAME"
      else
        local hostname=`hostname -f`
      fi

      echo -e "\e[0;36mWelcome \e[1;36m${USERNAME:-$USER}\e[0;36m on \e[1;33m$hostname\e[0m"
      echo
    fi

    if has fortune && [ -z "$SSH_CONNECTION" ]; then
      fortune -acs -n $((LINES*40)) | awk '
        /^\(.+\)$/ { print "\033[1;32mfortune:" $0 "\033[0m"; next }
        /^%$/ { next }
        { print "  \033[0;32m" $0 "\033[0m" }
      '
      echo
    fi

    ls
    echo
    from -c 2>/dev/null | grep -v "There are 0 messages" && echo
    true # for _ps1_exit_status
  fi
}

# Clear the screen and show the prompt at the bottom
function down {
  local i
  for i in `seq 1 $(tput lines)`; do
    echo
  done
}

# Open a file with xdg-open
function open {
  xdg-open "$@" &>/dev/null
}

# Move a file or directory and replace it by a symlink to the new location
function mvln {
  if [ "$1" = "-n" ]; then
    local echo="echo"
    shift
  else
    local echo=""
  fi

  local source=`readlink -f "$1" 2>/dev/null`
  local target=`realpath "$2" 2>/dev/null`
  [ -z "$target" ] && target="$2"

  [ -z "$source" -o -z "$target" ] && echo "Usage: mvln [-n] SOURCE TARGET" && return 1
  [ ! -e "$source" ] && echo "$source: No such file or directory" && return 1
  [ "$source" = "$target" ] && echo "Source and target are the same." && return 1

  $echo mv -v "$source" "$target" || return 1

  if [ -d "$target" ]; then
    local name=`basename "$source"`
    $echo ln -sv "$target/$name" "$source"
  else
    $echo ln -sv "$target" "$source"
  fi
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
  [ "`type -t gvi`" = "alias" ] && unalias gvi

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
        (cd /; command gvim $options --remote "${args[@]}")

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

# Switch to dotfiles repository if no arguments are passed
function dotfiles {
  local path=`command dotfiles --path`

  if [ $# -eq 0 ]; then
    cd "$path"
  elif [[ "$1" =~ ^b(u(n(d(l(e)?)?)?)?)?$ ]]; then
    if [ -z "$2" ]; then
      cd "$path/vim/bundle"
    elif [ -d "$path/$2" ]; then
      cd "$path/$2"
    elif [ -d "$path/vim/bundle/$2" ]; then
      cd "$path/vim/bundle/$2"
    else
      cd "`command ls -d "$path/vim/bundle/$2"* "$path/$2"* 2>/dev/null | head -1`"
    fi
  else
    command dotfiles "$@"
  fi
}

# Switch project directories and run Git commands in them
function src {
  local src_dir=~/src

  case "$1" in
    list)
      if [ "$2" = "-a" ]; then
        local filter="."
        local options=
      else
        local filter="^(archive|upstream)/"
        local options="-v"
      fi

      find -L "$src_dir" -mindepth 1 -maxdepth 4 -type d -name .git | sed -r "s|^$src_dir/(.+)/\.git$|\1|" | egrep $options "$filter" | egrep -v "^dotfiles/.+" | sort

      return
      ;;
    each)
      shift
      for project in `src list`; do
        echo -e "# \e[0;36m$project\e[0m" | sed -r "s|$HOME|~|"
        (cd "$src_dir/$project" || exit 1; $@)

        local status=$?
        if [ $status -ne 0 ]; then
          echo -e "# \e[1;31mexit code $status\e[0m"
        fi
        echo
      done

      return
      ;;
    st|status|-a|'')
      [ "$1" != "-a" ] && shift

      local first=1
      local last=0

      echo
      for project in `src list "$@"`; do
        dir="$src_dir/$project"
        [ -d "$dir/.git" ] || continue

        local changes=`cd "$dir"; git status -s | grep -c .`
        local unmerged=`cd "$dir"; git status | egrep "Your branch is (behind|ahead)"`

        if [ $changes -gt 0 -o -n "$unmerged" ]; then
          local label="changes"

          if [ $changes -gt 0 ]; then
            local label="changes"
            [ $changes -eq 1 ] && label="change"
            label="$changes\e[1;37m $label"
          elif echo "$unmerged" | grep -q ahead; then
            label="Unpublished commits"
          else
            label="Unmerged commits"
          fi

          [ -z "$first" ] && echo
          echo -e " \e[1;32m>\e[1;37m \e[1;33m$label in \e[1;36m[`realpath "$dir"`]\e[0m" | sed -r "s|$HOME|~|"
          (cd "$dir" || exit 1; git -c color.ui=always status | sed -r 's/^/    /')
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
      ;;
  esac

  local project="$1"
  shift

  if [ -d "$project" ]; then
    local path="$project"
  else
    local path="$src_dir/$project"
  fi

  if [ -z "$project" ]; then
    echo "Usage: src PROJECT [COMMAND] [ARGS]"
    echo "       src list [-a]"
    echo "       src status [-a]"
    return 255
  fi

  if [ ! -e "$path" ]; then
    local first_match=`src list -a | fgrep -m1 "$project"`
    if [ -n "$first_match" ]; then
      path="$src_dir/$first_match"
    else
      echo "$path does not exist"
      return 1
    fi
  fi

  if [ "$1" = "--path" ]; then
    echo "$path"
    return
  elif [ -f "$path" ]; then
    sensible-editor "$path"
    return
  elif [ ! -d "$path" ]; then
    echo "Unsupported path $path"
    return
  fi

  case "$1" in
    '')
      cd "$path"
      return
      ;;
    -e)
      command="$2"
      shift 2
      ;;
    @*)
      command="mux $1"
      shift
      ;;
    vi|vim|gvi|gvim|sensible-vim|rake|rails|cap|mux|shore)
      command="$1"
      shift
      ;;
    *)
      command="git"
      ;;
  esac

  (cd "$path" && $command "$@")
}

# Helper to create an alias for src with Git completion
function src_alias {
  local alias="$1"
  local project="$2"
  shift 2

  alias $alias="src $project $@"
  __git_edit_complete $alias _git `src "$project" --path`
}

# Selecta wrappers
if has selecta; then
  function selecta_wrapper {
    local command="$1"
    shift

    local selection=`"$@" | selecta`
    [ -n "$selection" ] && "$command" "$selection"
  }

  alias ssrc='selecta_wrapper src src list'
fi

# Sudo wrapper for systemctl
function systemctl {
  local command="systemctl"

  if [[ "$1" =~ (start|stop|reload|enable|disable) ]]; then
    command="sudo systemctl"
  fi

  command $command "$@"
}
