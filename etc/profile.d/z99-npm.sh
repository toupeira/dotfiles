# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# Add wrappers for all npm executables
for file in `find ~/{src,www}/*/node_modules/*/bin -maxdepth 1 -type f -executable 2>/dev/null | sort`; do
  name=`basename $file`
  if ! type -t "$name" >/dev/null; then
    eval "function $name { run_npm \"$file\"; }"
  fi
done

function run_npm {
  local file="$1"
  local name=`basename "$file"`
  local bin=`ls "node_modules/*/bin/$name" 2>/dev/null | head -1`

  if [ -x "$bin" ]; then
    command "$bin" "$@"
  else
    local dir=`echo "$file" | sed -r 's|node_modules/.*||'`
    inode=`stat -c %i .`
    cd "$dir"
    [ "`stat -c %i .`" != $inode ] && pwd

    command "$file" "$@"
    cd "$OLDPWD"
  fi
}
