# Helper to check for commands
function has {
  which "$1" &>/dev/null
}

# Define helper variable for interactive and login shells
[ -n "$BASH_VERSION" -a -n "$PS1" ] && BASH_INTERACTIVE=1
[ -n "$BASH_INTERACTIVE" ] && shopt -q login_shell && BASH_LOGIN=1

# Check for login shell and Linux
[ -n "$BASH_LOGIN" -a "`uname -s`" = "Linux" ] || return

SSH_BYOBU=~/.byobu/.ssh-agent

if [ -z "$SSH_AUTH_SOCK" ]; then
  # Load SSH agent if necessary
  exec ssh-agent -- bash --login
elif [ -z "$STY" -a -n "$SSH_CONNECTION" -a -d ~/.byobu ] && has screen; then
  # Load Screen for SSH sessions

  # Maintain link to SSH agent
  if [ -S "$SSH_AUTH_SOCK" ] && [ ! -h "$SSH_AUTH_SOCK" ]; then
    ln -sf "$SSH_AUTH_SOCK" "$SSH_BYOBU"
  fi
  export SSH_AUTH_SOCK="$SSH_BYOBU"

  exec screen -qRR
elif ! readlink -f "$SSH_BYOBU" &>/dev/null; then
  rm -f "$SSH_BYOBU"
fi

unset SSH_BYOBU
