# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# Automatically use sudo for administrative RubyGems commands
function __sudo_gem {
  local version="$1"
  local command="gem$version"
  shift

  which "$command" &>/dev/null || command="gem"

  if [ -d /var/lib/gems ]; then
    local home="/var/lib/gems/$version/"
  elif [ -d /opt/ruby-enterprise/lib/ruby/gems ]; then
    local home="/opt/ruby-enterprise/lib/ruby/gems/$version/"
  else
    echo "Can't find Gem path on this system!"
    return 1
  fi

  if [[ "$1" =~ (install|uninstall|update|cleanup) ]]; then
    command="sudo GEM_HOME=$home $command"
  fi

  GEM_HOME=$home $command "$@"
}

# Aliases for different Ruby versions
alias gem="__sudo_gem 1.8"
alias gem1.8="__sudo_gem 1.8"
alias gem1.9.1="__sudo_gem 1.9.1"

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
  local bin=`command ls .bundle/ruby/*/bin/"$name" 2>/dev/null | head -1`
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
