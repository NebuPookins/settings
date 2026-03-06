#!/bin/bash

# Function to send notification
send_notification() {
    notify-send --app-name="Hourly Work Reminder" --icon="file:///usr/share/icons/breeze/applets/256/org.kde.plasma.analogclock.svg" "Work Reminder" "Stay on track with your tasks!"

    # Play a sound using paplay if available
    if command -v paplay &> /dev/null; then
      paplay /usr/share/sounds/freedesktop/stereo/complete.oga
    else
      echo "paplay not available, sound won't be played."
    fi
}

# Function to calculate sleep time until the next hour
time_until_next_hour() {
    current_minute=$(date +%-M) # Remove leading zero
    current_second=$(date +%-S) # Remove leading zero
    minutes_until_next_hour=$((60 - current_minute))
    seconds_until_next_hour=$((minutes_until_next_hour * 60 - current_second))
    echo $seconds_until_next_hour
}

# Infinite loop to send notification every hour on the hour
while true
do
    current_hour=$(date +%-H) # Removes leading zero from hour

    send_notification

    # Exit the loop after sending the 11 PM notification
    echo "Current hour is $current_hour"
    if [[ "$current_hour" -eq 23 ]]; then
        notify-send "Work Reminder" "It's 11 PM. Exiting the script."
        exit 0
    fi

    # Sleep until the next hour
    sleep_seconds=$(time_until_next_hour)
    echo "Sleeping for $sleep_seconds seconds..."
    sleep $sleep_seconds
done

