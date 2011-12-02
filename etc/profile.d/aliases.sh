# Check for interactive bash
[ -z "$BASH_INTERACTIVE" ] && return

# ls
which dircolors &>/dev/null && eval `dircolors -b`
alias ls='ls --color'
alias ll='ls -lh'
alias l='ls -A'

# basic aliases
alias @='start'
alias ..='cd ..'
alias ...='cd ../..'
alias cd_='cd "$_"'
alias back='cd "$OLDPWD"'
alias cp='cp -v'
alias mv='mv -v'
alias rm='rm -v'
alias du='du -ch'
alias df='df -h'
alias ln='ln -svfi'
alias rgrep='grep -r --exclude=.svn --exclude=.git --exclude=.*.swp'
alias cgrep='GREP_OPTIONS= grep'
alias mime='file -i'
alias killbg='kill -9 %1'

# debian aliases
alias pkget='aptitude -Z install'
alias pkgpurge='aptitude -Z purge'
alias pkgremove='aptitude -Z remove'
alias pkglist='dpkg -L'
alias pkgname='apt-cache pkgnames'
alias pkgsearch='apt-cache search'
function pkgwhich {
  dpkg -S `which "$1"`
}

# sudo aliases
if which sudo &>/dev/null; then
  # file management
  alias sucp='sudo cp -vi'
  alias sumv='sudo mv -vi'
  alias surm='sudo rm -v'
  alias suln='sudo ln -svfi'
  alias sush='sudo bash --login'
  alias suown='sudo chown -vR root:root'
  alias sumod='sudo chmod -vR 644'

  # vim
  alias suvi='sudo vim'
  alias sugvi='sudo gvim'
  alias sudiff='sudo vimdiff'
  alias sugdiff='sudo gvimdiff'
  alias visudo='sudo visudo'

  # debian tools
  alias apt-get='sudo apt-get'
  alias aptitude='sudo aptitude'
  alias pkginstall='sudo dpkg -i'
  alias dpkg-reconfigure='sudo dpkg-reconfigure'
  alias update-alternatives='sudo update-alternatives'
  alias invoke-rc.d='sudo invoke-rc.d'
  alias update-rc.d='sudo update-rc.d'
  alias make-kpkg='sudo make-kpkg'

  # system tools
  alias init='sudo init'
  alias modprobe='sudo modprobe'
  alias rmmod='sudo rmmod'
  alias iptables='sudo iptables'
  alias ip6tables='sudo ip6tables'
  alias tcpdump='sudo tcpdump'
  alias ethtool='sudo ethtool'
  alias lsop='sudo lsof -ni | grep --color=never LISTEN | egrep --color=auto "^[^ ]+"'
  alias jnettop='sudo jnettop'
  alias ifconfig='sudo ifconfig'
  alias ifup='sudo ifup'
  alias ifdown='sudo ifdown'
  alias tcpkill='sudo tcpkill'
  alias fdisk='sudo fdisk'
  alias parted='sudo parted'
  alias lvm='sudo lvm'
fi

# program aliases
alias fortune='fortune -a'
alias thesaurus='dict -d moby-thes'
alias pstree='pstree -GUh'
alias ftrace='strace -fe trace=file'
alias ptrace='strace -fe trace=process'
alias fetchmail='fetchmail -v'
alias wget='wget -c'
alias gvi='gvim'
alias gcc='gcc -Wall'
alias sbcl='rlwrap sbcl'
alias ocaml='rlwrap ocaml'
alias gcc='gcc -Wall'
alias cadaver='EDITOR="gvim -f" cadaver'
alias psgrep='pgrep -fl'
alias dt='dotfiles'
which ack-grep &>/dev/null && alias ack='ack-grep'
which xdg-open &>/dev/null && alias open='xdg-open'
