export PATH="/usr/sbin:/usr/bin:/sbin:/bin:/usr/local/sbin:/usr/local/bin:/usr/games"

if [ -d /var/lib/gems/1.8/bin ]; then
  export PATH="$PATH:/var/lib/gems/1.8/bin"
fi

if [ -d ~/bin ]; then
  export PATH="$PATH":~/bin
fi

# Add OS X system paths
if [ -x /usr/libexec/path_helper ]; then
  eval `/usr/libexec/path_helper -s`
fi

if has sensible-vim; then
  export EDITOR="sensible-vim"
else
  export EDITOR="vim"
fi

export GIT_EDITOR="vim"
export PAGER="less"

export LESS="-iRM"
export GREP_OPTIONS="-i --color=auto --exclude=.svn --exclude=.git --exclude=.*.swp"
export IRB_HISTORY_SIZE=3000
export RI="--format ansi --no-pager"
