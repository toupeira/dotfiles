#!/bin/bash

[ -n "$BASH_INTERACTIVE" ] || return

# Load asdf
if [ -d ~/.asdf ]; then
  export ASDF_NODEJS_LEGACY_FILE_DYNAMIC_STRATEGY="latest_available"

  . ~/.asdf/asdf.sh
fi

# Add yarn global packages
if [ -d ~/.config/yarn/global/node_modules/.bin ]; then
  export PATH="$PATH:$HOME/.config/yarn/global/node_modules/.bin"
fi

# Setup kubectl completions
if has kubectl; then
  eval "$( kubectl completion bash )"

  alias k='kubectl'
  complete -o default -F __start_kubectl k
fi
