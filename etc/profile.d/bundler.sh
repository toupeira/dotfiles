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

# Add bundle exec wrappers for all executables
for file in `find ~/{src,www}/*/.bundle/ruby/*/bin -maxdepth 1 -type f -executable 2>/dev/null | sort`; do
  name=`basename $file`
  if ! type -t "$name" >/dev/null; then
    dir=`echo "$file" | sed -r 's|\.bundle/.*||'`
    eval "function $name { inode=\`stat -c %i .\`; cd \"$dir\"; [ \"\`stat -c %i .\`\" != \$inode ] && pwd; bundle exec \"$name\" \"\$@\"; cd \"\$OLDPWD\"; }"
  fi
done
