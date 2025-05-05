#!/bin/bash
# shellcheck disable=SC2034

export FZF_DEFAULT_COMMAND="fdfind --color always --max-results 99999"
export FZF_DEFAULT_OPTS="
  --ansi
  --cycle
  --filepath-word
  --highlight-line
  --history '$HOME/.local/state/history/fzf'
  --info inline-right
  --layout default
  --no-height
  --no-separator
  --prompt '» '
  --preview-window 'right,50%,hidden,<60(up,70%,hidden)'

  --color dark
  --color prompt:#82b1ff
  --color header:#82b1ff
  --color info:240
  --color hl:#d7ffaf:bold
  --color hl+:#ecffd9
  --color gutter:-1

  --bind alt-a:toggle-all
  --bind ctrl-n:down
  --bind ctrl-p:up
  --bind ctrl-f:page-down
  --bind ctrl-b:page-up
  --bind down:next-history
  --bind up:previous-history
  --bind ctrl-/:toggle-preview
  --bind alt-e:preview-down
  --bind alt-y:preview-up
  --bind alt-f:preview-half-page-down
  --bind alt-b:preview-half-page-up
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

# Setup FZF keybindings and completions.
# Use Ctrl-F instead of Ctrl-T to complete files
eval "$( fzf --bash | sed -r 's/\C-t/\C-f/g' )"

# Switch branches with Ctrl-s
bind '"\C-s": " \C-e\C-ugit switch-branch\C-m"'

# Complete from tmux panes with Ctrl-t
[ "$TMUX" ] && bind -x '"\C-t":__tmux_complete'
function __tmux_complete {
  local words=()
  local pane
  for pane in $( tmux list-panes -a -F '#D' ); do
    words=( "${words[@]}"
      $( tmux capture-pane -p -t "$pane" | grep -Eio '\w[-_.:/@[:alnum:]]{1,}\w' )
    )
  done

  result=$(
    printf '%s\n' "${words[@]}" \ | sort | uniq | \
      fzf --bind "ctrl-f:unbind(ctrl-f)+reload($FZF_CTRL_T_COMMAND)"
  )

  [ "$result" ] && tmux send-keys "$result"
}

# Use settings from keybindings for default path and dir completions too
_fzf_compgen_path() { $FZF_CTRL_T_COMMAND; }
_fzf_compgen_dir() { $FZF_ALT_C_COMMAND; }

# Show timestamps for history
# https://github.com/junegunn/fzf/issues/1049
__fzf_history__() {
  local output opts
  opts="--wrap --height ${FZF_TMUX_HEIGHT:-40%} --bind=ctrl-r:toggle-sort --wrap-sign '                             ↳ ' ${FZF_DEFAULT_OPTS-} --scheme=history ${FZF_CTRL_R_OPTS-} -n4.. +m +s --tac --ansi"
  output=$( builtin history | FZF_DEFAULT_OPTS="$opts" $(__fzfcmd) --query "$READLINE_LINE" ) || return
  READLINE_LINE=${output#*\[*\] }
  if [[ -z "$READLINE_POINT" ]]; then
    echo "$READLINE_LINE"
  else
    READLINE_POINT=0x7fffffff
  fi
}
