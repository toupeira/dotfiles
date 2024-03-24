# vim: foldmethod=marker

# load bash dotfiles

BASH_INTERACTIVE=1
BASH_LOGIN=1

has() {
  command -v "$1" &>/dev/null
}

. /etc/profile.d/001-environment.sh
. /etc/profile.d/aliases.sh
. /etc/profile.d/functions.sh
. /etc/profile.d/fzf.sh
. /etc/profile.d/ruby.sh

. ~/.profile

# oh-my-zsh setup

ZSH=~/.zsh/oh-my-zsh
ZSH_CUSTOM=~/.zsh
ZSH_THEME="powerlevel10k/powerlevel10k"
ZSH_DISABLE_COMPFIX=true

CASE_SENSITIVE="false"
HYPHEN_INSENSITIVE="true"
COMPLETION_WAITING_DOTS="false"
HISTFILE=~/.local/state/history/zsh
HIST_STAMPS="yyyy-mm-dd"

zstyle ':omz:update' mode disabled

plugins=(
  asdf
  fzf
  sudo
)

FZF_BASE=/etc/dotfiles/fzf

. $ZSH/oh-my-zsh.sh

# keybindings

bindkey '\ef' emacs-forward-word
