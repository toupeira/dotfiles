export PATH=/usr/sbin:/usr/bin:/sbin:/bin:/usr/local/sbin:/usr/local/bin:/usr/games:/var/lib/gems/1.8/bin:~/bin
export CDPATH=.:~

export EDITOR="vim"
export PAGER="less"

export LESS="-iRM"
export GREP_OPTIONS="-i --color=auto --exclude=.svn --exclude=.git --exclude=.*.swp"
export ACK_OPTIONS="--smart-case --ignore-dir=tmp/cache --ignore-dir=tmp/files"
export IRB_HISTORY_SIZE=3000
export RI="--format ansi --no-pager"

# Add system paths
if [ -x /usr/libexec/path_helper ]; then
  eval `/usr/libexec/path_helper -s`
fi
