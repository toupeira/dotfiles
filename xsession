#!/bin/bash

[ -f ~/.Xmodmap ] && xmodmap ~/.Xmodmap

# make Dropbox behave
dropbox=~/.config/autostart/dropbox.desktop
if [ -f "$dropbox" ]; then
  sed -ri 's/(dropbox start) -i/\1/' "$dropbox"
fi

# run the session
x-session-manager

# clean up
rm -rf ~/.local/share/Trash &
# if [ -f ~/.xsession-errors ]; then
#   mv ~/.xsession-errors ~/.xsession-errors.old
# fi

ql quit &>/dev/null &

if pgrep -x deluged; then
  (
    timeout 5 deluge-console halt
    killall deluged
    killall -9 deluged
  ) &>/dev/null &
fi

# wait for all processes to finish
wait

exit 0
