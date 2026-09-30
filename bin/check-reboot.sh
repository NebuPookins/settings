#!/bin/sh

# Script which checks if the kernel is up to date. If it's not up to date, you
# may want to reboot your system.
#
# On Arch, upgrading a kernel package removes the old version's
# /usr/lib/modules/<version>/kernel directory, so the running kernel is out of
# date exactly when that directory is gone. This works regardless of how many
# kernels (e.g. linux and linux-lts) are installed side by side. The kernel/
# subdirectory is checked rather than its parent because leftover DKMS modules
# can keep the parent alive. Nothing is reported when no kernel is installed
# at all (e.g. inside a container running the host's kernel).
#
# If you're using fish shell, you may want to have this script run on every
# shell. To do so, edit ~/.config/fish/fish.config and run this script from
# there.

MODULES_DIR=/usr/lib/modules
CURRENT_KERNEL=`uname -r`
set -- "${MODULES_DIR}"/*/kernel
if [ -d "$1" ] && [ ! -d "${MODULES_DIR}/${CURRENT_KERNEL}/kernel" ]; then
	INSTALLED=""
	for DIR in "$@"; do
		VERSION="${DIR%/kernel}"
		INSTALLED="${INSTALLED:+${INSTALLED}, }${VERSION##*/}"
	done
	echo "Kernel ${CURRENT_KERNEL} out of date. Installed: ${INSTALLED}. Reboot required."
fi
