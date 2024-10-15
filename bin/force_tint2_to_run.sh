#!/bin/bash

# Infinite loop to keep checking tint2
while true
do
    # Check if tint2 is running
    if ! pgrep -x "tint2" > /dev/null
    then
        echo "$(date): tint2 is not running. Restarting..."
        tint2
    else
        echo "$(date): tint2 is running."
    fi

    sleep 1
done

