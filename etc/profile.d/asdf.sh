#!/bin/bash

# Load asdf
if [ -d ~/.asdf ]; then
  export ASDF_DIR=~/.asdf
  . ~/.asdf/asdf.sh
  export PATH
fi

# Add yarn global packages
if [ -d ~/.yarn/bin ]; then
  export PATH="$PATH":~/.yarn/bin
fi

[ "$BASH_INTERACTIVE" ] || return

# Setup kubectl completions
if has kubectl; then
  eval "$( kubectl completion bash )"

  alias k='kubectl'
  complete -o default -F __start_kubectl k
fi
