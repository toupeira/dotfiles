#!/bin/bash
# shellcheck disable=SC2034

export FZF_DEFAULT_COMMAND="fdfind --color always --max-results 99999"
export FZF_DEFAULT_OPTS="
  --color=dark,prompt:#82b1ff,header:#82b1ff,info:240,hl:bold,gutter:-1
  --ansi --cycle --filepath-word --layout default --no-height --no-separator
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
FZF_ALT_C_COMMAND="$FZF_DEFAULT_COMMAND --type d"

FZF_CTRL_R_OPTS="
  --prompt 'History» '
  --history '$HOME/.local/state/history/fzf-history'
  --preview 'echo {} | sed -r \"s/^[0-9]*\\t*//\"'
  --preview-window 'default,up,5,hidden,wrap'
"
FZF_CTRL_T_OPTS="
  --preview 'fzf-preview.sh {}'
  --bind 'start:transform-prompt(echo \${PWD/#\$HOME/\~}/)'
  --bind 'ctrl-g:unbind(ctrl-g)+reload($FZF_CTRL_T_COMMAND --unrestricted --exclude .git)+transform-header(echo -e \"\\e[0;33m(\\e[1;33msearching all\\e[0;33m)\")'
"
FZF_ALT_C_OPTS="
  --history '$HOME/.local/state/history/fzf-cd'
  --preview 'tree -C {}'
  --bind 'start:transform-prompt(echo \${PWD/#\$HOME/\~}/)'
  --bind 'ctrl-g:unbind(ctrl-g)+reload($FZF_ALT_C_COMMAND --unrestricted --exclude .git)+transform-header(echo -e \"\\e[0;33m(\\e[1;33msearching all\\e[0;33m)\")'
"

if [ "$BASH_VERSION" ]; then
  eval "$( mise exec -- fzf --bash )"

  bind '"\C-s": " \C-e\C-ugit switch-branch\C-m"'
elif [ "$ZSH_VERSION" ]; then
  eval "$( mise exec -- fzf --zsh )"
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
