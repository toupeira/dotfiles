# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# Add wrappers for all bundler executables
for file in `find ~/{src,www}/*/.bundle/ruby/*/bin -maxdepth 1 -type f -executable 2>/dev/null | sort`; do
  name=`basename $file`
  if ! type -t "$name" >/dev/null; then
    eval "function $name { run_bundler \"$file\" \"\$@\"; }"
  fi
done

function run_bundler {
  local file="$1"
  local name=`basename "$file"`
  local bin=".bundle/ruby/1.8/bin/$name"
  shift

  if [ -x "$bin" ]; then
    bundle exec "$bin" "$@"
  else
    local dir=`echo "$file" | sed -r 's|\.bundle/.*||'`
    inode=`stat -c %i .`
    cd "$dir"
    [ "`stat -c %i .`" != $inode ] && pwd

    bundle exec "$name" "$@"
    cd "$OLDPWD"
  fi
}
