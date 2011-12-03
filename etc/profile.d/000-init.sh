# Define helper variable for interactive and login shells
[ -n "$BASH_VERSION" -a -n "$PS1" ] && BASH_INTERACTIVE=1
[ -n "$BASH_INTERACTIVE" ] && shopt -q login_shell && BASH_LOGIN=1

# Check for login shell and Linux
[ -z "$BASH_LOGIN" -o "`uname -s`" != "Linux" ] && return

if [ -z "$SSH_AUTH_SOCK" ]; then
  # Load SSH agent if necessary
  exec ssh-agent -- bash --login
elif [ -z "$STY" -a -n "$SSH_CONNECTION" -a -x /usr/bin/screen -a -d ~/.byobu ]; then
  # Load Screen for SSH sessions

  # Maintain link to SSH agent
  if [ -S "$SSH_AUTH_SOCK" ] && [ ! -h "$SSH_AUTH_SOCK" ]; then
    ln -sf "$SSH_AUTH_SOCK" ~/.byobu/.ssh-agent
  fi
  export SSH_AUTH_SOCK=~/.byobu/.ssh-agent

  exec screen -qRR
fi
