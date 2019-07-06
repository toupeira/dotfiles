# Helper to check for commands
has() {
  which "$1" &>/dev/null
}

# Define helper variables for interactive and login shells
[ -n "$BASH_VERSION" -a -n "$PS1" ] && BASH_INTERACTIVE=1
[ -n "$BASH_INTERACTIVE" ] && shopt -q login_shell && BASH_LOGIN=1

# Only continue in login shells
[ -n "$BASH_LOGIN" ] || return

# Tweak globbing
shopt -s extglob
shopt -s globstar

# Disable flow control (Ctrl-S/Q)
stty -ixon

if [ -z "$TMUX" -a -n "$SSH_CONNECTION" -a -x ~/bin/tmux-reattach ]; then
  # Load tmux for SSH sessions if it's set up
  exec ~/bin/tmux-reattach
fi
