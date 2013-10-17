#!/bin/sh

eval $(ssh-agent)
dropboxd &

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

case $1 in
#  gnome)
#    exec gnome-session
#    ;;
  cinnamon)
    exec cinnamon-session
    ;;
  xfce)
    exec startxfce4
    ;;
  *)
    trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 6 --transparent true --alpha 0 --tint 0x000000 --height 16 &
    volumeicon &
    /usr/bin/xscreensaver -no-splash &
    exec xmonad
    ;;
esac

