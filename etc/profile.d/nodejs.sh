# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# load nenv
if [ -d ~/.nenv ]; then
  export PATH="$HOME/.nenv/bin:$PATH"
  eval "$(nenv init - --no-rehash)"
fi

alias sunpm='sudo npm -g'
