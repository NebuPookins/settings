#!/bin/bash

# Monitor earlyoom journal and send desktop notifications

# Use a named pipe to avoid missing events during script restart (optional)
# We'll just tail -f and parse.

# Matches earlyoom's kill line, which looks like:
#   sending SIGTERM to process 41799 uid 1000 "brave": oom_score 155, oom_score_adj 300, VmRSS 2371 MiB, cmdline "/opt/brave-bin/brave"
regex='sending SIG(TERM|KILL) to process ([0-9]+) uid [0-9]+ "([^"]+)".*cmdline "([^"]*)"'

journalctl -u earlyoom -o cat -f --since="now" | while IFS= read -r line; do
    if [[ "$line" =~ $regex ]]; then
        signal="SIG${BASH_REMATCH[1]}"
        pid="${BASH_REMATCH[2]}"
        name="${BASH_REMATCH[3]}"
        cmdline="${BASH_REMATCH[4]}"
        notify-send -u critical -t 10000 "earlyoom sent $signal to $name" \
            "$name (PID $pid)"$'\n'"Command: $cmdline"
        paplay /usr/share/sounds/freedesktop/stereo/complete.oga 2>/dev/null
    fi
done