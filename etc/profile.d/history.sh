# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# History settings
shopt -s histappend
export HISTSIZE=5000
export HISTFILESIZE=5000
export HISTCONTROL="ignorespace"
