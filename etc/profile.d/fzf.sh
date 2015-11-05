# Check for interactive bash and fzf
[ -n "$BASH_INTERACTIVE" ] && has fzf || return

export FZF_TMUX=0
export FZF_DEFAULT_COMMAND='ag -l -g ""'
export FZF_DEFAULT_OPTS='-x -m'
export FZF_COMPLETION_TRIGGER='//'

. "$( dotfiles --path )/fzf/shell/completion.bash"

_fzf_orig_completion_ping='_ssh'
_fzf_orig_completion_telnet='_ssh'
_fzf_orig_completion_host='_ssh'
_fzf_orig_completion_nc='_ssh'
_fzf_orig_completion_curl='_ssh'

complete -F _fzf_complete_ssh ping telnet host nc curl
complete -o bashdefault -o default -F _fzf_path_completion ag bun
