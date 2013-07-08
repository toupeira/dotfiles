# Helper to check for commands
function has {
  which "$1" &>/dev/null
}

# Define helper variable for interactive and login shells
[ -n "$BASH_VERSION" -a -n "$PS1" ] && BASH_INTERACTIVE=1
[ -n "$BASH_INTERACTIVE" ] && shopt -q login_shell && BASH_LOGIN=1

# Only continue in login shells
[ -n "$BASH_LOGIN" ] || return

# Tweak globbing
shopt -s extglob
shopt -s globstar

# Only continue on Linux
[ "`uname -s`" = "Linux" ] || return

# Set up Screen / Byobu
SSH_BYOBU=~/.byobu/.ssh-agent

if [ -z "$SSH_AUTH_SOCK" ]; then
  # Try to connect to a running SSH agent
  socket=`ls -t /tmp/ssh-*/agent.[0-9]* 2>/dev/null | head -1`
  if [ -S "$socket" -a -O "$socket" ]; then
    echo "Found SSH agent"
    export SSH_AUTH_SOCK="$socket"
    export SSH_AGENT_PID=${SSH_AUTH_SOCK##*.}
  elif [ -n "$SSH_CONNECTION" -o -z "$DISPLAY" ]; then
    # Start a new SSH agent for SSH connections and local console sessions
    echo "Starting SSH agent"
    exec ssh-agent -- bash --login
  elif [ "$UID" != "0" ]; then
    # If there's no SSH agent running in a desktop environment there's something wrong
    echo "Couldn't find SSH agent"
    read -t 0.4
  fi
elif [ -z "$STY" -a -n "$SSH_CONNECTION" -a -d ~/.byobu ] && has screen; then
  # Load screen for SSH sessions if Byobu is set up

  # Maintain link to SSH agent for Byobu
  if [ -S "$SSH_AUTH_SOCK" ] && [ ! -h "$SSH_AUTH_SOCK" ]; then
    ln -sf "$SSH_AUTH_SOCK" "$SSH_BYOBU"
  fi
  export SSH_AUTH_SOCK="$SSH_BYOBU"

  exec screen -qRR
elif ! readlink -f "$SSH_BYOBU" &>/dev/null; then
  rm -f "$SSH_BYOBU"
fi

unset SSH_BYOBU
