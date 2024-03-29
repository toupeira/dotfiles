#!/bin/bash

if [ "$BASH_INTERACTIVE" ]; then
  eval "$( mise activate bash )"
  eval "$( mise completion bash )"
else
  # Add shims to $PATH for desktop usage
  eval "$( mise activate --shims )"
fi

if [ "$HOSTNAME" = "snafu" ]; then
  export MISE_DISABLE_TOOLS='ruby,nodejs'
fi
