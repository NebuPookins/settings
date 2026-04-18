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
# [ -x /usr/bin/keepass ] && keepass &
[ -x /usr/bin/pidgin ] && pidgin &
[ -x /usr/bin/xcompmgr ] && xcompmgr -f -n &

# Adds a cursor.
xsetroot -cursor_name left_ptr

# Fix monitor layouts
fix-screens


# Start ibus, which allows for alternative input methods (IMEs).
# export GTK_IM_MODULE=ibus
# export QT_IM_MODULE=ibus
# export XMODIFIERS=@im=ibus
# ibus-daemon -drxR

# Use to show a password prompt when a GUI application needs root permissions
[ -x /usr/bin/lxqt-policykit-agent ] && /usr/bin/lxqt-policykit-agent &

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
	i3)
		exec i3
		;;
	kodi) # Media center thingy
		exec kodi-standalone
		;;
	*)
		# Set up trayer (for dropbox, skype, etc.) in top right corner. Height of 25 is trial and error to what matches xmobar.
		# trayer --monitor "primary" --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 4 --transparent true --alpha 0.5 --tint 0x000000 --height 25 &
		force_tint2_to_run.sh &
		#~/bin/hourly_work_reminder.sh &
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

