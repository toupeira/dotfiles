# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

if [ -d ~/.tmuxifier ]; then
  . ~/.tmuxifier/tmuxifier/init.sh
fi

function mux {
  TMUX= roxterm --maximize -e ~/.tmuxifier/bin/tmuxifier s "$@" &>/dev/null &
}
