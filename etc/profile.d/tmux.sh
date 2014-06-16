# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

if [ -d ~/.tmuxifier ]; then
  . ~/.tmuxifier/tmuxifier/init.sh
fi
