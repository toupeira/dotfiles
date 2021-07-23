#!/bin/bash

# Add yarn global packages
if [ -d ~/.config/yarn/global/bin ]; then
  export PATH="$PATH:$HOME/.config/yarn/global/bin"
fi
