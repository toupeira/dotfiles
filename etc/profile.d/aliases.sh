# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# dotfiles aliases
alias @='start '
alias dt='dotfiles'
has git-edit && alias ed='git-edit'

# general shell aliases
has dircolors && eval `dircolors -b`
alias ls='ls --color --quoting-style=literal'
alias ll='ls -lh'
alias l='ls -A'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias cd_='cd "$_"'
alias back='cd "$OLDPWD"'
alias cp='cp -v'
alias mv='mv -v'
alias rm='rm -v'
alias du='du -ch'
alias df='df -h'
alias ln='ln -svfi'
alias mime='file -i'
alias killbg='kill -9 %1'
alias xcopy='xclip -selection clipboard -i'
alias xpaste='xclip -selection clipboard -o'
alias grep='grep -i --color=auto --exclude=.svn --exclude=.git --exclude=.*.swp'
alias egrep='grep -E'
alias fgrep='grep -F'
alias rgrep='grep -r'
alias ssh.direct='ssh -o ControlPath=none'

if has http; then
  alias GET='http get'
  alias HEAD='http -h get'
  alias POST='http post'
  alias PUT='http put'
fi

# system administration
alias sctl='systemctl'
alias jctl='journalctl'
alias pstree='pstree -GUh'
alias ftrace='strace -fe trace=file'
alias ptrace='strace -fe trace=process'
alias psgrep='pgrep -fla'
alias pskill='pkill -fe'
alias smem='smem -kt'
alias watch='watch -cd -n 1 '

# package managment
alias pkget='aptitude -Z install'
alias pkgpurge='aptitude -Z purge'
alias pkgremove='aptitude -Z remove'
alias pkglist='dpkg -L'
alias pkgname='apt-cache pkgnames'
alias pkgsearch='apt-cache search'
alias pkgshow='aptitude show'

# development
alias vi='vim'
alias gvi='gvim'
alias :e='vim'
alias :E='vim'
if has sensible-vim; then
  alias vim='sensible-vim'
  alias gvim='VIMCMD=gvim sensible-vim'
  alias vip='VIMCMD=vip sensible-vim'
fi

alias ag='ag --smart-case'
alias irb='pry'
alias pyserve='python -m SimpleHTTPServer 8080'
alias xvfb-run='xvfb-run -a -s "-screen 0 1280x8192x24" --'

# sudo aliases
if has sudo; then
  function _sudo_aliases {
    while [ $# -gt 0 ]; do
      eval "alias $1='sudo $1'"
      shift
    done
  }

  # expand aliases in arguments
  alias sudo='sudo '

  # vim
  alias suvi='sudo TMUX=$TMUX vim'
  alias sudiff='sudo vimdiff'
  alias visudo='sudo visudo'

  # file management
  alias sucp='sudo cp -vi'
  alias sumv='sudo mv -vi'
  alias surm='sudo rm -v'
  alias suln='sudo ln -svfi'
  alias sush='sudo -i'
  alias suown='sudo chown -vR root:root'
  alias sumod='sudo chmod -vR 644'

  # debian tools
  _sudo_aliases \
    apt-get aptitude dpkg-reconfigure \
    update-alternatives update-rc.d invoke-rc.d

  alias pkginstall='sudo dpkg -i'
  has eatmydata && alias aptitude='sudo eatmydata aptitude'

  # system tools
  _sudo_aliases \
    modprobe rmmod ifup ifdown ethtool iptables ip6tables \
    tcpdump jnettop fdisk parted lvm docker fastboot

  alias lsop='sudo lsof -ni | grep --color=never LISTEN | egrep --color=auto "^[^ ]+|:\w+"'
fi
