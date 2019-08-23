# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# Prompt configuration
PS1_USER="\u"
PS1_HOST=" "
[ -z "$SSH_CONNECTION" ] && [ "$USER" = "toupeira" -o "$USER" = "mak" ] && PS1_USER="λ"
[ -n "$SSH_CONNECTION" ] && PS1_HOST="@\h "
[ -n "$EMACS" ] && PS1_USER="" && PS1_HOST=""
[ "$UID" = "0" ] && PS1_USER="\[\e[1;31m\]$PS1_USER"

PS1="\[\e[1;35m\]\$(_prompt_jobs)\[\e[0m\]\[\e[1;30m\]$PS1_USER\[\e[1;33m\]$PS1_HOST\[\e[0;36m\][\[\e[1;36m\]\$(_prompt_path)\[\e[0;36m\]]\[\e[0m\] "

# Prompt helpers
function _prompt_path {
  local pwd="$PWD"
  pwd=${pwd/#$HOME/\~}
  pwd=${pwd/#\~\/src\/gitlab\//🦊 }
  pwd=${pwd/#\~\/src\/gitlab/🦊}
  pwd=${pwd/#\/etc\/dotfiles\//⚙️  }
  pwd=${pwd/#\/etc\/dotfiles/⚙️ }

  echo "$pwd"
}

function _prompt_jobs {
  local jobs=`jobs | grep -Evc '(mux store|autojump)'`

  if [ $jobs -gt 0 ]; then
    printf '[%d job%s] ' $jobs "`([ $jobs -eq 1 ] || echo -n s)`"
  fi
}

function _prompt_exit_status {
  [ -n "$CYGWIN" -o "$CONQUE" ] && return

  if [ -n "$_last_status" ] && [ $_last_status -gt 0 ]; then
    tput sc
    local column=$((COLUMNS-${#_last_status}-3))

    tput cup $LINES $column
    printf '\e[1;30m[\e[1;31m%s\e[1;30m]\e[0m '  $_last_status
    tput rc
  fi
}

# Use Git prompt if available
if type __git_ps1 &>/dev/null; then
  GIT_PS1_SHOWDIRTYSTATE=1
  GIT_PS1_SHOWSTASHSTATE=1
  GIT_PS1_SHOWUNTRACKEDFILES=1
  GIT_PS1_SHOWUPSTREAM='auto git'

  GIT_PS1_SUBSTITUTES="
    s/\\*/○/;
    s/\\+/●/;
    s/%/‽/;
    s/\\\$/$/;
    s/=//;
    s/<>/ ⇵/;
    s/>/ ↑/;
    s/</ ↓/;
    s/\\bmaster\\b /🛡️ /;
    s/\\bmaster\\b/🛡️ /;
    s/([-[:alnum:]]{16})[-[:alnum:]]+/\\1…/;
  "

  GIT_PS1='$(__git_ps1 "\[\e[0;32m\]❰\[\e[1;32m\]%s\[\e[0;32m\]❱\[\e[0m\] " | sed -r "$GIT_PS1_SUBSTITUTES")'
  SUDO_PS1=$PS1
  PS1=$PS1$GIT_PS1
fi

# Show user, hostname and pwd in window title
if [[ "$TERM" =~ ^(rxvt|xterm|tmux|screen) ]]; then
  if [ -n "$SSH_CONNECTION" ]; then
    _hostname="$USER@$HOSTNAME: "
  else
    unset _hostname
  fi

  PROMPT_COMMAND='_pwd=$( _prompt_path ); _last_status=$?;'

  if [ -n "$TMUX" ]; then
    PROMPT_COMMAND=$PROMPT_COMMAND'[ "$PWD" != "$_last_pwd" ] && mux store; _last_pwd="$PWD"; echo -ne "\e]0;'$_hostname'$_pwd\007\ek$_pwd\e\\"'
  else
    PROMPT_COMMAND=$PROMPT_COMMAND'echo -ne "\e]1;'$_hostname'$_pwd\007\e]2;'$_hostname'$_pwd\007"'
  fi

  unset _hostname
fi

if has autojump; then
  . /usr/share/autojump/autojump.bash

  # don't output the jumped directory
  eval "_j() $( declare -f j | tail -n +2 | sed -r 's/echo .*output.*/:/' )"

  function j {
    if [ $# -gt 0 ]; then
      _j "$@"
      return;
    fi

    local paths=~/.local/share/autojump/autojump.txt
    local out=$(
      cat $paths 2>/dev/null \
        | sort -nr \
        | cut -f2 \
        | sed -r "s#^$HOME#~#" \
        | fzf +s --no-multi --prompt 'Jump> ' --expect alt-d
    )

    mapfile -t out <<< "$out"
    local key="${out[0]}"
    local path=$( echo "${out[1]}" | sed -r "s#^~#$HOME#" )

    if [ "$key" = "alt-d" ]; then
      echo "$( grep -v $'\t'"$path$" "$paths" )" > "$paths"
      j
    elif [ -n "$path" ]; then
      cd "$path"
    fi
  }
fi
