#!/bin/bash
# shellcheck disable=SC2034

export FZF_DEFAULT_COMMAND="fdfind --hidden --color always"
export FZF_DEFAULT_OPTS="
  --color=dark,gutter:-1
  --ansi --multi --cycle --filepath-word --inline-info --layout default --no-height --no-separator
  --history '$HOME/.local/state/history/fzf'
  --history-size 10000
  --prompt '» '
  --preview-window 'right,50%,hidden,<60(up,70%,hidden)'
  --bind 'ctrl-a:toggle-all,ctrl-n:down,ctrl-p:up,down:next-history,up:previous-history,ctrl-/:toggle-preview,ctrl-e:preview-down,ctrl-y:preview-up,ctrl-f:preview-half-page-down,ctrl-b:preview-half-page-up'
"

[ "$BASH_INTERACTIVE" ] || return

FZF_COMPLETION_TRIGGER='//'

FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
FZF_ALT_C_COMMAND="$FZF_DEFAULT_COMMAND --type d --unrestricted --exclude .git"

FZF_CTRL_R_OPTS="
  --prompt 'History» '
  --history '$HOME/.local/state/history/fzf-shell-history'
  --preview 'echo {} | sed -r \"s/^[0-9]*\\t*//\"'
  --preview-window 'default,up,5,hidden,wrap'
"
FZF_CTRL_T_OPTS="
  --preview 'fzf-preview {}'
"
FZF_ALT_C_OPTS="
  --prompt 'Dir» '
  --history '$HOME/.local/state/history/fzf-cd'
  --preview 'tree -C {}'
"

_fzf_compgen_path() { $FZF_CTRL_T_COMMAND; }
_fzf_compgen_dir() { $FZF_ALT_C_COMMAND; }

if [ "$BASH_VERSION" ]; then
  eval "$( fzf --bash )"

  bind '"\C-s": " \C-e\C-ugit switch-branch\C-m"'
elif [ "$ZSH_VERSION" ]; then
  eval "$( fzf --zsh )"
fi
