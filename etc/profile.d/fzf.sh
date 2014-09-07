# Check for interactive bash and fzf
[ -n "$BASH_INTERACTIVE" ] && has fzf || return

export FZF_DEFAULT_COMMAND='ag -l -g ""'
export FZF_DEFAULT_OPTS='-x -m'
export FZF_COMPLETION_TRIGGER='//'

. "`dotfiles --path`/fzf/fzf-completion.bash"

_fzf_orig_completion_ping='_ssh'
_fzf_orig_completion_telnet='_ssh'
_fzf_orig_completion_host='_ssh'
_fzf_orig_completion_nc='_ssh'

complete -F _fzf_ssh_completion ping telnet host nc
