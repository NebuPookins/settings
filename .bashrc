#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Bash won't get SIGWINCH if another process is in the foreground.
# Enable checkwinsize so that bash will check the terminal size when
# it regains control.
# http://cnswww.cns.cwru.edu/~chet/bash/FAQ (E11)
shopt -s checkwinsize

# Enable history appending instead of overwriting.
shopt -s histappend

# Enable color in various commands.
alias df="df -h"
alias diff="colordiff" #requires colordiff package to be installed.
alias dir="dir --color=auto"
alias dmesg="dmesg --color"
alias grep='grep -n --color=auto'
alias ls='ls -hF --color=auto'
man() {
	env LESS_TERMCAP_mb=$'\E[01;31m' \
	LESS_TERMCAP_md=$'\E[01;38;5;74m' \
	LESS_TERMCAP_me=$'\E[0m' \
	LESS_TERMCAP_se=$'\E[0m' \
	
	LESS_TERMCAP_ue=$'\E[0m' \
	LESS_TERMCAP_us=$'\E[04;38;5;146m' \
	man "$@"
	}

# Set prompt. http://maketecheasier.com/8-useful-and-interesting-bash-prompts/2009/09/04
PS1="\n\[\033[1;37m\]\342\224\214($(if [[ ${EUID} == 0 ]]; then echo '\[\033[01;31m\]\h'; else echo '\[\033[01;34m\]\u@\h'; fi)\[\033[1;37m\])\342\224\200(\$(if [[ \$? == 0 ]]; then echo \"\[\033[01;32m\]\342\234\223\"; else echo \"\[\033[01;31m\]\342\234\227\"; fi)\[\033[1;37m\])\342\224\200(\[\033[1;34m\]\@ \d\[\033[1;37m\])\[\033[1;37m\]\n\342\224\224\342\224\200(\[\033[1;32m\]\w\[\033[1;37m\])\342\224\200(\[\033[1;32m\]\$(ls -1 | wc -l | sed 's: ::g') files, \$(ls -lah | grep -m 1 total | sed 's/total //')b\[\033[1;37m\])\342\224\200> \[\033[0m\]"

# Sets default editors to nano
export EDITOR=nano
export SVN_EDITOR=nano

# easily lock screen
alias lock=cinnamon-screensaver-lock-dialog #Needs cinnamon-screensaver-lock-dialog package

# find where diskspace is going
alias diskspace="du -S | sort -n -r | less"

# make custom scripts accessible
PATH="$HOME/bin:$PATH"

# Try to enable the auto-completion (type: "pacman -S bash-completion" to install it).
[ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion

# Requires command-not-found to be installed from AUR
# See also: https://wiki.archlinux.org/index.php/Bash#The_.22command_not_found.22_hook
if [ -r /etc/profile.d/cnf.sh ]
	then
		source /etc/profile.d/cnf.sh
fi

# cd and ls in one
cl() {
	if [[ -d "$1" ]]; then
		cd "$1"
		ls
	else
		echo "bash: cl: '$1': Directory not found"
	fi
}
