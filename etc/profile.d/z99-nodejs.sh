# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# Add wrappers for all npm executables
for command in `find ~/{src,www}/*/node_modules/*/bin -maxdepth 1 -type f -executable -printf "%f\n" 2>/dev/null | sort | uniq`; do
  eval "function $command { npm_exec \"$command\" \"\$@\"; }"
done
unset command

function npm_exec {
  local pwd="$PWD"
  local command="$1"
  local bin=`npm bin`/"$command"
  shift

  if [ -x "$bin" ]; then
    command "$bin" "$@"
  else
    command "$command" "$@"
  fi
}
