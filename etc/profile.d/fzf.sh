# shellcheck shell=bash

# Check for interactive bash and fzf
[ -n "$BASH_INTERACTIVE" ] && has fzf || return

export FZF_TMUX=0
export FZF_COMPLETION_TRIGGER='//'

export FZF_DEFAULT_COMMAND="fdfind --type f --type l --hidden --color always --ignore-file /etc/dotfiles/ignore"
export FZF_DEFAULT_OPTS="
  --ansi --multi --cycle --filepath-word --inline-info --layout default --no-height
  --history $HOME/.fzf_history
  --prompt '» '
  --preview-window right:70%:hidden
  --bind 'ctrl-o:toggle-preview,ctrl-j:down,ctrl-n:down,ctrl-p:up,down:next-history,up:previous-history'"

export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND --type d --type l"
export FZF_ALT_C_COMMAND="${FZF_DEFAULT_COMMAND/--type f/--type d}"

export FZF_CTRL_R_OPTS="
  --prompt 'History> '
  --preview 'echo {} | cut -d\\  -f4-'
  --preview-window up:3:hidden:wrap"
export FZF_CTRL_T_OPTS="
  --prompt 'Path> '
  --preview 'fzf-preview {}'"
export FZF_ALT_C_OPTS="
  --prompt 'Directory> '
  --preview 'tree -C {}'"

_fzf_compgen_path() { $FZF_CTRL_T_COMMAND; }
_fzf_compgen_dir() { $FZF_ALT_C_COMMAND; }

. /usr/share/doc/fzf/examples/completion.bash
. /usr/share/doc/fzf/examples/key-bindings.bash

bind '"\C-b": " \C-e\C-ugit c\C-m"'

_fzf_orig_completion_ping='_ssh'
_fzf_orig_completion_telnet='_ssh'
_fzf_orig_completion_host='_ssh'
_fzf_orig_completion_nc='_ssh'
_fzf_orig_completion_curl='_ssh'

complete -F _fzf_complete_ssh ping telnet host nc curl
complete -o bashdefault -o default -F _fzf_path_completion rg
