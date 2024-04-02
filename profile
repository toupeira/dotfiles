#!/bin/bash

if [ -d ~/.profile.d ]; then
  for i in ~/.profile.d/*.sh; do
    if [ -r "$i" ]; then
      . "$i"
    fi
  done
  unset i
fi

[ -n "$BASH_INTERACTIVE" ] && login_banner
