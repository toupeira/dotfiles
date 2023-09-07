#!/bin/bash

[ -n "$BASH_INTERACTIVE" ] && has fzf || return

export FZF_TMUX=0
export FZF_COMPLETION_TRIGGER='//'

export FZF_DEFAULT_COMMAND="fdfind --type f --type l --hidden --color always --ignore-file /etc/dotfiles/ignore"
export FZF_DEFAULT_OPTS="
  --color=dark,gutter:-1
  --ansi --multi --cycle --filepath-word --inline-info --layout default --no-height
  --history $HOME/.fzf_history
  --prompt '» '
  --preview-window right:70%:hidden
  --bind 'ctrl-a:toggle-all,ctrl-o:toggle-preview,ctrl-j:down,ctrl-n:down,ctrl-p:up,down:next-history,up:previous-history'
"

export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND --type d --type l"
export FZF_ALT_C_COMMAND="${FZF_DEFAULT_COMMAND/--type f/--type d}"

export FZF_CTRL_R_OPTS="
  --prompt 'History> '
  --preview 'echo {} | cut -d\\  -f4-'
  --preview-window up:3:hidden:wrap
"
export FZF_CTRL_T_OPTS="
  --prompt 'Path> '
  --preview 'fzf-preview {}'
"
export FZF_ALT_C_OPTS="
  --prompt 'Directory> '
  --preview 'tree -C {}'
"

_fzf_compgen_path() { $FZF_CTRL_T_COMMAND; }
_fzf_compgen_dir() { $FZF_ALT_C_COMMAND; }

if [ -n "$BASH_VERSION" ]; then
  . /etc/dotfiles/fzf/shell/completion.bash
  . /etc/dotfiles/fzf/shell/key-bindings.bash

  bind '"\C-s": " \C-e\C-ugit switch-branch\C-m"'
fi
