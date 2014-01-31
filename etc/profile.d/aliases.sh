# Check for interactive bash
[ -n "$BASH_INTERACTIVE" ] || return

# ls
has dircolors && eval `dircolors -b`
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
alias ag='ag --smart-case'

# debian aliases
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
  alias vim='CMD=vim sensible-vim'
  alias gvim='sensible-vim'
fi

# sudo aliases
if has sudo; then
  # expand aliases in arguments
  alias sudo='sudo '

  # vim
  alias suvi='sudo vim'
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
  alias pkginstall='sudo dpkg -i'
  alias dpkg-reconfigure='sudo dpkg-reconfigure'
  alias update-alternatives='sudo update-alternatives'
  alias make-kpkg='sudo make-kpkg'
  alias update-rc.d='sudo update-rc.d'
  alias invoke-rc.d='sudo invoke-rc.d'

  # ubuntu tools
  if has initctl; then
    alias start='sudo start'
    alias stop='sudo stop'
    alias reload='sudo reload'
    alias restart='sudo restart'
  fi

  # system tools
  alias init='sudo init'
  alias modprobe='sudo modprobe'
  alias rmmod='sudo rmmod'
  alias iptables='sudo iptables'
  alias ip6tables='sudo ip6tables'
  alias tcpdump='sudo tcpdump'
  alias ethtool='sudo ethtool'
  alias lsop='sudo lsof -ni | grep --color=never LISTEN | egrep --color=auto "^[^ ]+|:\w+"'
  alias jnettop='sudo jnettop'
  alias ifconfig='sudo ifconfig'
  alias ifup='sudo ifup'
  alias ifdown='sudo ifdown'
  alias tcpkill='sudo tcpkill'
  alias fdisk='sudo fdisk'
  alias parted='sudo parted'
  alias lvm='sudo lvm'

  # rails
  alias passenger-status='sudo passenger-status'
  alias passenger-memory-stats='sudo passenger-memory-stats'
fi

# program aliases
alias s='git status'
alias thesaurus='dict -d moby-thes'
alias pstree='pstree -GUh'
alias ftrace='strace -fe trace=file'
alias ptrace='strace -fe trace=process'
alias psgrep='pgrep -fl'
alias dt='dotfiles'
alias smem='smem -kt'
alias watch='watch -c'
has xdg-open && alias open='xdg-open'
has 7z && ! has rar && alias rar='7z'
has git-edit && alias ed='git-edit'

if has rlwrap; then
  has sbcl && alias sbcl='rlwrap sbcl'
  has ocaml && alias ocaml='rlwrap ocaml'
fi
