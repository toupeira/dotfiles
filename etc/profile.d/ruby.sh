# Add rbenv binstubs
if [ -d ~/.rbenv ]; then
  export PATH="$HOME/.rbenv/bin:$PATH"
fi

# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# Load rbenv
if [ -d ~/.rbenv ]; then
  eval "$(rbenv init - --no-rehash)"
fi

# sudo wrapper for RubyGems
function _gem_exec {
  local command="$1"
  shift

  if rbenv version | grep -q ^system && [[ "$1" =~ (install|uninstall|update|clean|pristine) ]]; then
    command="sudo $command"
  fi

  $command "$@"
}

alias gem='_gem_exec gem'
alias sugem='command sudo gem'

# gem aliases
has rubocop && alias rubocop='rubocop -D'

# Automatically use rails/rake commands
function r {
  if grep -q ' rails ([5-9]\.' Gemfile.lock &>/dev/null || [[ "$1" =~ ^(s|server|c|console|g|generate|d|destroy|r|runner|db|dbconsole|new)$ ]]; then
    rails "$@"
  else
    rake "$@"
  fi
}
