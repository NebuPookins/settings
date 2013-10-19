#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

#Set default editor to nano, instead of vi or some crap like that. -Nebu
export EDITOR=nano

# Sets default SVN editor to nano
# export SVN_EDITOR=nano

#Set GREP to use color by default.
export GREP_OPTIONS='--color=auto'

# Sets svn diff to emit color, and automatically page
svn() {
	/usr/bin/svn "${@}" | colordiff
}
