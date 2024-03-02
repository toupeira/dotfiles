# vim: foldmethod=marker

# enable powerlevel10k instant prompt
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# load bash dotfiles {{{

BASH_INTERACTIVE=1
BASH_LOGIN=1

has() {
  command -v "$1" &>/dev/null
}

. /etc/profile.d/001-environment.sh
. /etc/profile.d/aliases.sh
. /etc/profile.d/functions.sh
. /etc/profile.d/fzf.sh

# }}}

# oh-my-zsh setup {{{

ZSH=~/.zsh/oh-my-zsh
ZSH_CUSTOM=~/.zsh
ZSH_THEME="powerlevel10k/powerlevel10k"

CASE_SENSITIVE="false"
HYPHEN_INSENSITIVE="true"

zstyle ':omz:update' mode disabled  # disable automatic updates

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
COMPLETION_WAITING_DOTS="false"

HIST_STAMPS="yyyy-mm-dd"

# }}}

# oh-my-sh plugins {{{

plugins=(
  asdf
  bundler
  fzf
  git
  sudo
  tmux
)

FZF_BASE=/etc/dotfiles/fzf
ZSH_THEME_GIT_PROMPT_CACHE=true

if [ -z "$SUDO_USER" ] && [ "$TERM" != "linux" ]; then
  ZSH_TMUX_AUTOSTART=true
fi

zstyle ':omz:plugins:bundler' aliases no
zstyle ':omz:plugins:git'     aliases no
zstyle ':omz:plugins:tmux'    aliases no

# }}}

. $ZSH/oh-my-zsh.sh
