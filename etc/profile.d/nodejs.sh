export NODE_PATH=/usr/lib/node_modules:/usr/share/javascript

# Add nenv binstubs
if [ -d ~/.nenv ]; then
  export PATH="$PATH:$HOME/.nenv/bin"
fi

# Add yarn global packages
if [ -d ~/.yarn/bin ]; then
  export PATH="$PATH:$HOME/.yarn/bin"
fi

# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# Load nenv
if [ -d ~/.nenv ]; then
  eval "$(nenv init - --no-rehash)"
fi

alias node='node --preserve-symlinks'
alias sunpm='sudo npm -g'

function ng {
  local command="ng"
  [ -f yarn.lock ] && command="yarn exec -s -- $command"
  command $command "$@"
}
