# Helper to check for commands
has() {
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

# Disable flow control (Ctrl-S/Q)
stty -ixon

if [ -z "$SSH_AUTH_SOCK" -a "$UID" != "0" ]; then
  # Try to connect to a running SSH agent
  socket=`command ls -t /tmp/ssh-*/agent.[0-9]* 2>/dev/null | head -1`
  if [ -S "$socket" -a -O "$socket" ]; then
    export SSH_AUTH_SOCK="$socket"
    export SSH_AGENT_PID=${SSH_AUTH_SOCK##*.}
  elif [ -n "$SSH_CONNECTION" -o -z "$DISPLAY" ]; then
    # Start a new SSH agent for SSH connections and local console sessions
    [ "$TERM" = "linux" ] || echo "Starting SSH agent"
    exec ssh-agent -- bash --login
  else
    # If there's no SSH agent running in a desktop environment there's something wrong
    echo "Couldn't find SSH agent"
    read -t 0.4
  fi
fi

# Set up tmux
SSH_AGENT_TMUX=~/.tmux/.ssh-agent

if [ -z "$TMUX" -a -n "$SSH_CONNECTION" -a -x ~/bin/tmux-reattach ]; then
  # Load tmux for SSH sessions if it's set up

  # Maintain link to SSH agent for tmux
  if [ -S "$SSH_AUTH_SOCK" ] && [ ! -h "$SSH_AUTH_SOCK" ]; then
    ln -sf "$SSH_AUTH_SOCK" "$SSH_AGENT_TMUX"
  fi
  export SSH_AUTH_SOCK="$SSH_AGENT_TMUX"

  # Try to attach to a detached session, start a new one otherwise
  exec ~/bin/tmux-reattach
elif ! readlink -f "$SSH_AGENT_TMUX" &>/dev/null; then
  rm -f "$SSH_AGENT_TMUX"
fi

unset SSH_AGENT_TMUX
