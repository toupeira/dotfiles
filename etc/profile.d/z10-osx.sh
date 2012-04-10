# Check for interactive bash and OS X
[ -z "$BASH_VERSION" -o -z "$PS1" -o "`uname -s`" != "Darwin" ] && return

# Homebrew aliases
alias find='gfind'
alias sed='gsed'
alias gvim='mvim'
alias gvimdiff='mvimdiff'

# Overwrite existing aliases to use GNU versions
alias ls='gls --color'
alias du='gdu -ch'
alias df='df -h'
alias ln='gln -svfi'

# Don't load SSH key
function __load_key { :; }

# Gem doesn't need the sudo wrapper
unset gem
