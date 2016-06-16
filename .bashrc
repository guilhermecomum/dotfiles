# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# force ignoredups and ignorespace
HISTCONTROL=ignoreboth
export HISTSIZE=1000

# Timestamps in history file
export HISTTIMEFORMAT="%d/%m/%y %T "

# append to the history file, don't overwrite it
shopt -s histappend

# check the window size after each command and, if necessary, update
# the values of LINES and COLUMNS.
shopt -s checkwinsize

if [[ -z "$TMUX" ]] ;then
    ID="`tmux ls | grep -vm1 attached | cut -d: -f1`" # get the id of a deattached session
    if [[ -z "$ID" ]] ;then # if not available create a new one
        tmux new-session
    else
        tmux attach-session -t "$ID" # if available attach to it
    fi
fi

# Alias definitions.
if [ -f ~/.bash.d/aliases ]; then
    . ~/.bash.d/aliases
fi

# I want to know when things break!
ulimit -c unlimited

# General stuff
export EMAIL='guerrinha@comum.org'
export EDITOR="emacsc"

# PS1, yeah it's a big deal!
red=$'\e[1;31;40m'
grn=$'\e[1;32;40m'
yel=$'\e[1;33;40m'
blu=$'\e[1;34;40m'
mag=$'\e[1;35;40m'
cyn=$'\e[1;36;40m'
end=$'\e[0m'

# Since the venv name written in PS1 by the virtualenv's postactivate
# script won't have colors, the original customized value is saved in
# the variable `ORIG_PS1` and re-exported in the postactivate script.
export ORIG_PS1="\[$red\]♥ \[$blu\]\W \[$yel\](\$(__git_ps1 \"%s\"))\[$grn\] $ \[$end\]"
export PS1=$ORIG_PS1

# # Since the venv name written in PS1 by the virtualenv's postactivate
# # script won't have colors, the original customized value is saved in
# # the variable `ORIG_PS1` and re-exported in the postactivate script.
# export ORIG_PS1='\w\[\033[01;33m\]$(__git_ps1)\[\033[01;34m\] \$\[\033[00m\] '
# export PS1=$ORIG_PS1

# Extend path with reasonable directories
PATH="$HOME/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
export PATH

# Load platform specific settings
platform=$(uname 2>/dev/null | tr [A-Z] [a-z])
. $HOME/.bash.d/platform/$platform

# Load programming framework specific settings
. $HOME/.bash.d/lang/nodejs
