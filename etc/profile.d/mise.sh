#!/bin/bash

if [ "$BASH_INTERACTIVE" ]; then
  eval "$( mise activate bash )"
else
  eval "$( mise activate --shims )"
fi

if [ "$HOSTNAME" = "snafu" ]; then
  export MISE_DISABLE_TOOLS='ruby,node'
fi
