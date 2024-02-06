#!/bin/bash

# Add yarn global packages
if [ -d ~/.config/yarn/global/node_modules/.bin ]; then
  export PATH="$PATH:$HOME/.config/yarn/global/node_modules/.bin"
fi
