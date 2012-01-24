# Check for interactive bash and MySQL
[ -n "$BASH_INTERACTIVE" ] && has mysql || return

alias mysql='mysql -u root'

function mysql-ps {
  uptime
  mysqladmin status || return 1
  echo

  command="mysql -e 'SHOW FULL PROCESSLIST' -B | sort -k 5"
  if [ -n "$1" ]; then
    eval $command | egrep "$1"
  else
    eval $command | egrep -v "\sSleep\s"
  fi
}

function mysql-kill {
  mysql -e "KILL $1"
}

function mysql-top {
  while true; do
    clear
    mysql-ps
    sleep 1
  done
}
