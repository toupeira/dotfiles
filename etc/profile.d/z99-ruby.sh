# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# sudo wrapper for RubyGems
function _gem_exec {
  local command="$1"
  shift

  if [[ "$1" =~ (install|uninstall|update|clean|pristine) ]]; then
    command="sudo $command"
  fi

  $command "$@"
}

# Aliases for different Ruby versions
alias gem="_gem_exec /usr/bin/gem"
alias gem1.8="_gem_exec /usr/bin/gem1.8"
alias gem1.9.1="_gem_exec /usr/bin/gem1.9.1"

# Add wrappers for all bundler executables
for command in `find ~/{src,www}/*/.bundle/ruby/*/bin -maxdepth 1 -type f -executable -printf "%f\n" 2>/dev/null | sort | uniq`; do
  eval "function $command { bundle_exec \"$command\" \"\$@\"; }"
done
unset command

function bundle_exec {
  local pwd="$PWD"
  local command="$1"
  shift

  # override user name when running Capistrano (for Airbrake + NewRelic)
  if [ "$command" = "cap" -a "$USER" = "toupeira" ]; then
    export USER="markus"
  fi

  # look for a .bundle directory
  while [ ! -d "$pwd/.bundle" ]; do
    pwd=${pwd%/*}
    if [ -z "$pwd" ]; then
      # no bundle found, run the command from the system
      command "$command" "$@"
      return
    fi
  done

  if ls $pwd/.bundle/ruby/*/bin/$command &>/dev/null; then
    # run the command with bundler
    bundle exec "$command" "$@"
  else
    # run the command from the system
    command "$command" "$@"
  fi
}
