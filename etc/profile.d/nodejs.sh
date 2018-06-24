# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

export NODE_PATH=/usr/lib/nodejs:/usr/lib/node_modules:/usr/share/javascript

# load nenv
if [ -d ~/.nenv ]; then
  export PATH="$HOME/.nenv/bin:$PATH"
  eval "$(nenv init - --no-rehash)"
fi

# add yarn global packages
if [ -d ~/.yarn/bin ]; then
  export PATH="$PATH:$HOME/.yarn/bin"
fi

alias sunpm='sudo npm -g'

function ng {
  local command="ng"
  [ -f yarn.lock ] && command="yarn exec -s -- $command"
  command $command "$@"
}
