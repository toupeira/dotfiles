# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# Wrapper for RubyGems
function gem_exec {
  local version="$1"
  local command="gem$version"
  shift

  which "$command" &>/dev/null || command="gem"

  # use correct Gem path
  if [ -d /var/lib/gems ]; then
    local home="/var/lib/gems/$version/"
  elif [ -d /opt/ruby-enterprise/lib/ruby/gems ]; then
    local home="/opt/ruby-enterprise/lib/ruby/gems/$version/"
  else
    echo "Can't find Gem path on this system!"
    return 1
  fi

  # use sudo for administrative commands
  if [[ "$1" =~ (install|uninstall|update|clean) ]]; then
    command="sudo GEM_HOME=$home $command"
  fi

  GEM_HOME=$home $command "$@"
}

# Aliases for different Ruby versions
alias gem="gem_exec 1.8"
alias gem1.8="gem_exec 1.8"
alias gem1.9.1="gem_exec 1.9.1"
alias gem2.0="gem_exec 2.0"

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

  # pass the command to bundler
  bundle exec "$command" "$@"
}
