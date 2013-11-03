#!/bin/sh

# enables ssh-agent if it is installed (install openssh package to enable this)
[ -r /usr/bin/ssh-agent ] && . eval $(ssh-agent)

# Run these programs on startup
[ -r /usr/bin/dropboxd ] && . dropboxd &
[ -r /usr/bin/skype ] && . skype &
[ -r /usr/bin/keepass ] && . keepass &
[ -r /usr/bin/pidgin ] && . pidgin &

# Run system-wide scripts.
if [ -d /etc/X11/xinit/xinitrc.d ]; then
	for f in /etc/X11/xinit/xinitrc.d/*; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

# Adds a cursor.
xsetroot -cursor_name left_ptr

# Runs the correct environment.
case $1 in
	# gnome)
	# 	exec gnome-session
	# 	;;
	cinnamon)
		exec cinnamon-session
		;;
	xfce)
		exec startxfce4
		;;
	*)
		# Set up trayer (for dropbox, skype, etc.) in top right corner.
		trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 6 --transparent true --alpha 0 --tint 0x000000 --height 16 &
		# Shows a volume icon
		volumeicon &
		# Activate screensaver
		/usr/bin/xscreensaver -no-splash &
		# Show a random wallpaper (requires custom script)
		[ -r ~/bin/random-wallpaper.sh ] && ~/bin/random-wallpaper.sh &
		exec xmonad
		;;
esac

