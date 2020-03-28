# shellcheck shell=bash

# Add yarn global packages
if [ -d ~/.yarn/bin ]; then
  export PATH="$PATH:$HOME/.yarn/bin"
fi

# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

alias sunpm='sudo npm -g'
