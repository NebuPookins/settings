#!/bin/bash

# Monitor earlyoom journal and send desktop notifications

# Use a named pipe to avoid missing events during script restart (optional)
# We'll just tail -f and parse.

journalctl -u earlyoom -o cat -f --since="now" | while IFS= read -r line; do
    regex='sending SIG(TERM|KILL) to process ([0-9]+) uid [0-9]+ "([^"]+)".*cmdline "([^"]+)"'
    if [[ "$line" =~ $regex ]]; then
        pid="${BASH_REMATCH[1]}"
        name="${BASH_REMATCH[2]}"
        cmdline="${BASH_REMATCH[3]}"
        notify-send -u critical -t 10000 "earlyoom killed a process" \
            "$name (PID $pid)\nCommand: $cmdline"
        paplay /usr/share/sounds/freedesktop/stereo/complete.oga 2>/dev/null
    fi
done