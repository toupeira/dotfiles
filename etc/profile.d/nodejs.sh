#!/bin/bash

# Add yarn global packages
if [ -d ~/.yarn/bin ]; then
  export PATH="$PATH:$HOME/.yarn/bin"
fi
