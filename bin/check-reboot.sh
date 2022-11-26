#!/bin/sh

# Script which checks if the kernel is up to date. If it's not up to date, you
# may want to reboot your system.
#
# If you're using fish shell, you may want to have this script run on every
# shell. To do so, edit ~/.config/fish/fish.config and run this script from
# there.

NEXTLINE=0
FIND=""
for I in `file /boot/vmlinuz*`; do
	if [ ${NEXTLINE} -eq 1 ]; then
		FIND="${I}"
		NEXTLINE=0
	else
		if [ "${I}" = "version" ]; then NEXTLINE=1; fi
	fi
done
if [ ! "${FIND}" = "" ]; then
	CURRENT_KERNEL=`uname -r`
	if [ ! "${CURRENT_KERNEL}" = "${FIND}" ]; then
		echo "Kernel ${CURRENT_KERNEL} out of date. Latest is ${FIND}. Reboot required."
	fi
fi

