# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# ls
has dircolors && eval `dircolors -b`
alias ls='ls --color'
alias ll='ls -lh'
alias l='ls -A'

# general aliases
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
alias ag='ag --smart-case'
alias pstree='pstree -GUh'
alias ftrace='strace -fe trace=file'
alias ptrace='strace -fe trace=process'
alias psgrep='pgrep -fla'
alias pskill='pkill -fe'
alias smem='smem -kt'
alias watch='watch -cd -n 1 '
alias irb='pry'
alias pyserve='python -m SimpleHTTPServer 8080'
has 7z && ! has rar && alias rar='7z'

alias sctl='systemctl'
alias jctl='journalctl'

alias @='start '
alias dt='dotfiles'
has git-edit && alias ed='git-edit'

# package aliases
alias pkget='aptitude -Z install'
alias pkgpurge='aptitude -Z purge'
alias pkgremove='aptitude -Z remove'
alias pkglist='dpkg -L'
alias pkgname='apt-cache pkgnames'
alias pkgsearch='apt-cache search'

# vim aliases
alias vi='vim'
alias gvi='gvim'
if has sensible-vim; then
  alias vim='sensible-vim'
  alias gvim='CMD=gvim sensible-vim'
fi

# sudo aliases
if has sudo; then
  # expand aliases in arguments
  alias sudo='sudo '

  # vim
  alias suvi='sudo TMUX=$TMUX vim'
  alias sugvi='sudo gvim'
  alias sudiff='sudo vimdiff'
  alias sugdiff='sudo gvimdiff'
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
  alias apt-get='sudo apt-get'
  alias aptitude='sudo aptitude'
  has eatmydata && alias aptitude='sudo eatmydata aptitude'
  alias pkginstall='sudo dpkg -i'
  alias dpkg-reconfigure='sudo dpkg-reconfigure'
  alias update-alternatives='sudo update-alternatives'
  alias make-kpkg='sudo make-kpkg'
  alias update-rc.d='sudo update-rc.d'
  alias invoke-rc.d='sudo invoke-rc.d'

  # system tools
  alias modprobe='sudo modprobe'
  alias rmmod='sudo rmmod'
  alias iptables='sudo iptables'
  alias ip6tables='sudo ip6tables'
  alias tcpdump='sudo tcpdump'
  alias ethtool='sudo ethtool'
  alias lsop='sudo lsof -ni | grep --color=never LISTEN | egrep --color=auto "^[^ ]+|:\w+"'
  alias jnettop='sudo jnettop'
  alias ifup='sudo ifup'
  alias ifdown='sudo ifdown'
  alias tcpkill='sudo tcpkill'
  alias fdisk='sudo fdisk'
  alias parted='sudo parted'
  alias lvm='sudo lvm'
  alias docker='sudo docker'
  alias fastboot='sudo fastboot'
fi
