#!/usr/bin/env bash

# BEGIN BLOCK COPIED FROM DEFAULT XINIT AT /etc/X11/xinit/xinitrc
userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
	xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
	xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
	xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
	xmodmap "$usermodmap"
fi

# Run system-wide scripts.

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
	for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi
# END BLOCK COPIED FROM DEFAULT XINIT AT /etc/X11/xinit/xinitrc

# enables ssh-agent if it is installed (install openssh package to enable this)
# [ -r /usr/bin/ssh-agent ] && . eval $(ssh-agent)

# Run these programs on startup
[ -x /usr/bin/dropboxd ] && dropboxd &
[ -x /usr/bin/skype ] && skype &
[ -x /usr/bin/keepass ] keepass &
[ -x /usr/bin/pidgin ] pidgin &
[ -x /usr/bin/xcompmgr ] xcompmgr -f -n &

# Adds a cursor.
xsetroot -cursor_name left_ptr

case $1 in
	# gnome)
	# 	exec gnome-session
	# 	;;
	# cinnamon)
	# 	exec cinnamon-session
	# 	;;
	xfce)
		exec startxfce4
		;;
	*)
		# Set up trayer (for dropbox, skype, etc.) in top right corner.
		trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 6 --transparent true --alpha 0 --tint 0x000000 --height 16 &
		# Shows a volume icon
		# volumeicon &
		# # Activate screensaver
		# /usr/bin/xscreensaver -no-splash &
		# Show a random wallpaper (requires custom script)
		[ -x ~/bin/random-wallpaper.sh ] && ~/bin/random-wallpaper.sh &
		# dbus-launch is necessary to get evince to remember last opened page.
		exec dbus-launch --exit-with-session xmonad
		;;
esac

