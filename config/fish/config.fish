# This file should live in ~/.config/fish/config.fish

if status is-interactive
  set -gx SUDO_EDITOR vim
  set -gx EDITOR vim
  set -gx _JAVA_AWT_WM_NONREPARENTING 1
  fish_add_path ~/settings/bin/
  ~/settings/bin/check-reboot.sh
  echo
end
