#!/bin/bash

export PATH="/usr/sbin:$DOTFILES/bin:/usr/bin:/usr/local/sbin:/usr/local/bin:/usr/games"

for dir in ~/bin "$DOTFILES/packages/mason/bin"; do
  [ -d "$dir" ] && export PATH="$PATH:$dir"
done
unset dir

# terminal apps
export TERMINAL_EMULATOR="x-terminal-emulator"
export EDITOR="sensible-vim"
export PAGER="less"
export LESS="-iRMK --mouse"
export MANPAGER="sensible-vim +Man!"
export SYSTEMD_LESS="iRMKFX --mouse"
export RIPGREP_CONFIG_PATH=~/.config/rg/config
export BAT_THEME="TwoDark"

# desktop apps
if [ "$XDG_SESSION_TYPE" = "wayland" ] && [ ! "$QT_SCALE_FACTOR" ]; then
  export QT_SCALE_FACTOR=$( dconf read /org/gnome/desktop/interface/text-scaling-factor )
  QT_SCALE_FACTOR=${QT_SCALE_FACTOR:-1}
fi

# initialize mise
if [ -d ~/.config/mise ]; then
  if [ "$HOSTNAME" != "snafu" ]; then
    export MISE_ENV='development'
  fi

  if [ "$BASH_INTERACTIVE" ]; then
    eval "$( mise activate bash )"
  else
    PATH="$HOME/.local/share/mise/shims:$PATH"
  fi
fi
