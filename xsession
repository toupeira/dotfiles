#!/bin/bash

# run the session
x-session-manager

# clean up

rm -rf ~/.local/share/Trash &

if [ -p ~/.quodlibet/control ]; then
  timeout 3 quodlibet --quit &
fi

if pgrep -x deluged; then
  timeout 5 deluge-console halt &
fi

# wait for all processes to finish/abort
wait

exit 0
