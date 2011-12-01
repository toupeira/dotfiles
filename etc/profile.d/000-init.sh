# Define helper variable for interactive and login shells
[ -n "$BASH_VERSION" -a -n "$PS1" ] && BASH_INTERACTIVE=1
[ -n "$BASH_INTERACTIVE" ] && shopt -q login_shell && BASH_LOGIN=1

# Check for login shell and Linux
[ -z "$BASH_LOGIN" -o "`uname -s`" != "Linux" ] && return

if [ -z "$SSH_AUTH_SOCK" ]; then
  # Load SSH agent if necessary
  exec ssh-agent -- bash --login
elif [ -z "$STY" -a -n "$SSH_CONNECTION" -a -x /usr/bin/screen ]; then
  # Load Screen for SSH sessions
  exec screen -qRR
fi
