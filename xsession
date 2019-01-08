#!/bin/bash

# disable the Menu key
xmodmap -e 'keycode 135 = NoSymbol'

# make Dropbox behave
dropbox=~/.config/autostart/dropbox.desktop
if [ -f "$dropbox" ]; then
  sed -ri 's/(dropbox start) -i/\1/' "$dropbox"
fi

# run the session
x-session-manager

# clean up
rm -rf ~/.local/share/Trash &

killall -9 kodi.bin 2>/dev/null

if pgrep -x quodlibet; then
  ql quit
fi

if pgrep -x deluged; then
  timeout 5 deluge-console halt
  killall deluged
  killall -9 deluged
fi

if pgrep -x emacs; then
  emacsclient -e '(kill-emacs)'
fi

exit 0
