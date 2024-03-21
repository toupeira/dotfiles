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
