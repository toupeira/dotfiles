# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# History settings
shopt -s histappend
export HISTSIZE=10000
export HISTFILESIZE=10000
export HISTCONTROL="ignorespace"
