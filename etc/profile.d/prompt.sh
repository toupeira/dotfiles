# Check for interactive bash
[ -z "$BASH_INTERACTIVE" ] && return

# Prompt configuration
export PS1='\u@\h:\w\$ '

# Use Git prompt if available
if type __git_ps1 &>/dev/null; then
  export GIT_PS1_SHOWDIRTYSTATE=1
  export GIT_PS1_SHOWSTASHSTATE=1
  export GIT_PS1_SHOWUNTRACKEDFILES=1

  export GIT_PS1='$(__git_ps1 "[\[\033[1;32m\]%s\[\033[0m\]] ")'
  export SUDO_PS1=$PS1
  export PS1=$PS1$GIT_PS1
fi

# Set window titles when displaying prompt
if [[ "$TERM" =~ ^(rxvt|xterm-256color|screen-bce) ]]; then
  if [ "$STY" ]; then
    export PROMPT_COMMAND='_pwd=${PWD/$HOME/\~}; echo -ne "\033]0;$HOSTNAME:$_pwd\007\033k$_pwd\033\\"'
  elif [ "$SSH_CONNECTION" ]; then
    export PROMPT_COMMAND='_pwd=${PWD/$HOME/\~}; echo -ne "\033]1;@$HOSTNAME:$_pwd\007\033]2;$USER@$HOSTNAME:$_pwd\007"'
  else
    export PROMPT_COMMAND='_pwd=${PWD/$HOME/\~}; echo -ne "\033]1;$_pwd\007\033]2;$_pwd\007"'
  fi
fi
