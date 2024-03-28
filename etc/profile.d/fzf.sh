#!/bin/bash
# shellcheck disable=SC2034

export FZF_DEFAULT_COMMAND="fdfind --hidden --color always"
export FZF_DEFAULT_OPTS="
  --color=dark,gutter:-1
  --ansi --multi --cycle --filepath-word --layout default --no-height --no-separator
  --history '$HOME/.local/state/history/fzf'
  --prompt '» '
  --pointer '»'
  --marker '●'
  --info inline-right
  --preview-window 'right,50%,hidden,<60(up,70%,hidden)'
  --bind ctrl-a:toggle-all
  --bind ctrl-n:down
  --bind ctrl-p:up
  --bind down:next-history
  --bind up:previous-history
  --bind ctrl-/:toggle-preview
  --bind ctrl-e:preview-down
  --bind ctrl-y:preview-up
  --bind ctrl-f:preview-half-page-down
  --bind ctrl-b:preview-half-page-up
"

[ "$BASH_INTERACTIVE" ] || return

FZF_COMPLETION_TRIGGER='//'

FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
FZF_ALT_C_COMMAND="$FZF_DEFAULT_COMMAND --type d --unrestricted --exclude .git"

FZF_CTRL_R_OPTS="
  --prompt 'History» '
  --history '$HOME/.local/state/history/fzf-history'
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

if [ "$BASH_VERSION" ]; then
  eval "$( fzf --bash )"

  bind '"\C-s": " \C-e\C-ugit switch-branch\C-m"'
elif [ "$ZSH_VERSION" ]; then
  eval "$( fzf --zsh )"
fi

_fzf_compgen_path() { $FZF_CTRL_T_COMMAND; }
_fzf_compgen_dir() { $FZF_ALT_C_COMMAND; }

# Show timestamps for history
# https://github.com/junegunn/fzf/issues/1049
__fzf_history__() {
  local output opts
  opts="--height ${FZF_TMUX_HEIGHT:-40%} --bind=ctrl-z:ignore ${FZF_DEFAULT_OPTS-} --scheme=history ${FZF_CTRL_R_OPTS-} -n4.. +m +s --tac --ansi"
  output=$( builtin history | FZF_DEFAULT_OPTS="$opts" $(__fzfcmd) --query "$READLINE_LINE" ) || return
  READLINE_LINE=${output#*\[*\] }
  if [[ -z "$READLINE_POINT" ]]; then
    echo "$READLINE_LINE"
  else
    READLINE_POINT=0x7fffffff
  fi
}
