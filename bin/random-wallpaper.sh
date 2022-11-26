#!/bin/bash

WALLPAPER_DIR="/home/nebu/h/My Documents/My Pictures/Wallpapers/"

while true; do
	if [ -d "$WALLPAPER_DIR" ]; then
		feh --bg-fill --randomize "$WALLPAPER_DIR"
		sleep 15m
	else
		echo "$WALLPAPER_DIR does not seem to be a directory. Exiting."
		exit 1
	fi
done
