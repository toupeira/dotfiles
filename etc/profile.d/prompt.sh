# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# Prompt configuration
PS1_USER="\u"
PS1_HOST=""
[ -n "$SSH_CONNECTION" -o "$TERM" = "linux" ] && PS1_HOST="@\h"
[ "$UID" = "0" ] && PS1_USER="\[\e[1;31m\]$PS1_USER"

export PS1="\[\e[1;30m\]$PS1_USER\[\e[1;33m\]$PS1_HOST \[\e[0;36m\][\[\e[1;36m\]\w\[\e[0;36m\]]\[\e[0m\] \[\$(_prompt_exit_status)\]"

# Use Git prompt if available
if type __git_ps1 &>/dev/null; then
  export GIT_PS1_SHOWDIRTYSTATE=1
  export GIT_PS1_SHOWSTASHSTATE=1
  export GIT_PS1_SHOWUNTRACKEDFILES=1
  export GIT_PS1_SHOWUPSTREAM='auto git'

  GIT_PS1_SUBSTITUTES="
    s/=//;
    s/<>/ ⇵/;
    s/>/ ↑/;
    s/</ ↓/;
  "

  export GIT_PS1='$(__git_ps1 "\[\e[0;32m\][\[\e[1;32m\]%s\[\e[0;32m\]]\[\e[0m\] " | sed "$GIT_PS1_SUBSTITUTES")'
  export SUDO_PS1=$PS1
  export PS1=$PS1$GIT_PS1
fi

function _prompt_jobs {
  local jobs=`jobs | grep -v 'tmux setenv'`

  if [ -n "$jobs" ]; then
    tput sc
    tput cup 0 0
    echo -e "\e[1;35m`_repeat '=' $COLUMNS`\e[0m"
    echo "$jobs" | sed -r "s/.*/`_repeat ' ' $COLUMNS`/"
    echo -e "\e[1;35m`_repeat '=' $COLUMNS`\e[0m"
    tput cup 1 0
    echo "$jobs" | sed -r 's/^(.+)  (Stopped|Running) *(.*)$/ \o33[1;35m\1 \o33[0;35m[\2] \o33[1;35m\3\o33[0m/'
    tput rc
  fi
}

function _prompt_exit_status {
  local status=$?
  [ -n "$CYGWIN" -o "$CONQUE" ] && return

  if [ $status -gt 0 ]; then
    tput sc
    local column=$((COLUMNS-${#status}-3))

    tput cup $LINES $column
    printf '\e[1;30m[\e[1;31m%s\e[1;30m]\e[0m '  $status
    tput rc
  fi
}

# Show user, hostname and pwd in window title
if [[ "$TERM" =~ ^(rxvt|xterm|screen) ]]; then
  if [ -n "$SSH_CONNECTION" ]; then
    _hostname="$USER@$HOSTNAME: "
  else
    unset _hostname
  fi

  if [[ "$TERM" =~ ^screen ]]; then
    export PROMPT_COMMAND='_prompt_jobs; [ -n "$TMUX" ] && tmux setenv TMUXPWD_$(tmux display -p "#D" | tr -d %) "$PWD"; _pwd=${PWD/$HOME/\~}; echo -ne "\e]0;'$_hostname'$_pwd\007\ek$_pwd\e\\"'
  else
    export PROMPT_COMMAND='_pwd=${PWD/$HOME/\~}; echo -ne "\e]1;'$_hostname'$_pwd\007\e]2;'$_hostname'$_pwd\007"'
  fi

  unset _hostname
fi
