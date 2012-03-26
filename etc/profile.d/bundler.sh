# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# Check if another missing command handler is defined
if [ "`type -t command_not_found_handle`" = "function" ]; then
  echo "Warning: command_not_found_handle is already defined, skipping Bundler integration..."
  return
fi

function command_not_found_handle {
  local command="$1"
  local bin=".bundle/ruby/1.8/bin/$command"
  shift

  if [ -x "$bin" ]; then
    bundle exec "$bin" "$@"
  else
    echo "-bash: $command: command not found"
  fi
}
