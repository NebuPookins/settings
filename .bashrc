#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

# Sets default SVN editor to nano
export SVN_EDITOR=nano

# Sets svn diff to emit color, and automatically page
svn() {
	/usr/bin/svn "${@}" | colordiff | less -R
}
