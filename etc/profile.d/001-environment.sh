. /etc/default/locale
export LANG LC_MESSAGES LC_NUMERIC LC_CTYPE

export PATH="/usr/sbin:/usr/bin:/sbin:/bin:/usr/local/sbin:/usr/local/bin:/usr/games"
[ -d ~/bin ] && export PATH="$HOME/bin:$PATH"
[ -x /usr/libexec/path_helper ] && eval `/usr/libexec/path_helper -s`

export EDITOR="vim"
export GIT_EDITOR="vim"
export PAGER="less"
which sensible-vim >/dev/null && export EDITOR="sensible-vim"

export LESS="-iRM"
export GREP_OPTIONS="-i --color=auto --exclude=.svn --exclude=.git --exclude=.*.swp"
export IRB_HISTORY_SIZE=3000
export RI="--format ansi --no-pager"
export PGDATABASE="postgres"
export ANDROID_HOME=~/src/upstream/android-sdk-linux

# Colorize manpages
export LESS_TERMCAP_mb=$'\e[1;31m'
export LESS_TERMCAP_md=$'\e[1;32m'
export LESS_TERMCAP_us=$'\e[1;33m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_ue=$'\e[0m'
