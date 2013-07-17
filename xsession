#!/bin/bash

# run the session
x-session-manager

# clean up
rm -rf ~/.local/share/Trash &

if [ -p ~/.quodlibet/control ]; then
  (
    timeout 3 quodlibet --quit
    killall quodlibet
    killall -9 quodlibet
  ) &>/dev/null &
fi

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
