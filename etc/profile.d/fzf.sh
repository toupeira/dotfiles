#!/bin/bash

[ "$BASH_INTERACTIVE" ] && has fzf || return

export FZF_TMUX=0
export FZF_COMPLETION_TRIGGER='//'

export FZF_DEFAULT_COMMAND="fdfind --type f --type l --hidden --color always"
export FZF_DEFAULT_OPTS="
  --color=dark,gutter:-1
  --ansi --multi --cycle --filepath-word --inline-info --layout default --no-height --no-separator
  --history $HOME/.fzf_history
  --prompt '» '
  --preview-window 'right,50%,hidden,<60(up,80%,hidden)'
  --bind 'ctrl-a:toggle-all,ctrl-/:toggle-preview,ctrl-n:down,ctrl-p:up,down:next-history,up:previous-history'
"

export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND --type d --type l"
export FZF_ALT_C_COMMAND="${FZF_DEFAULT_COMMAND/--type f/--type d}"

export FZF_CTRL_R_OPTS="
  --prompt 'History» '
  --preview 'echo {} | sed -r \"s/^[0-9]*\\t*//\"'
  --preview-window 'default,up,5,hidden,wrap'
"
export FZF_CTRL_T_OPTS="
  --prompt 'Path» '
  --preview 'fzf-preview {}'
"
export FZF_ALT_C_OPTS="
  --prompt 'Directory» '
  --preview 'tree -C {}'
"

_fzf_compgen_path() { $FZF_CTRL_T_COMMAND; }
_fzf_compgen_dir() { $FZF_ALT_C_COMMAND; }

if [ "$BASH_VERSION" ]; then
  . /etc/dotfiles/fzf/shell/completion.bash
  . /etc/dotfiles/fzf/shell/key-bindings.bash

  bind '"\C-s": " \C-e\C-ugit switch-branch\C-m"'
fi
