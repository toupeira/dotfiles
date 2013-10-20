#!/bin/bash

# run the session
x-session-manager

# clean up
rm -rf ~/.local/share/Trash &

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
