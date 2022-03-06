# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# force ignoredups and ignorespace
HISTCONTROL=ignoreboth
export HISTSIZE=1000

# Timestamps in history file
export HISTTIMEFORMAT="%d/%m/%y %T "

# After each command, save and reload history
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"

#disable left ctrl
xmodmap -e "keycode 37="

# append to the history file, don't overwrite it
shopt -s histappend

# check the window size after each command and, if necessary, update
# the values of LINES and COLUMNS.
shopt -s checkwinsize

if [[ -z "$TMUX" ]] ;then
    SESSION="`tmux ls | wc -l`"
    if [[ "$SESSION" -ge 1 ]] ;then # if not available create a new one
        tmux attach
    else
        tmux new
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
export EDITOR="emacsclient -c"

# Randomize string (space-separated values).
function randomize_string () {
  echo $@ | tr " " "\n" | perl -MList::Util=shuffle -e 'print shuffle(<STDIN>);' | tr "\n" " "
}

# Extract a random item from a string (space-separated values).
function random_el () {
  local array=($(randomize_string $@))
  # Bash $RANDOM is terrible; use jot.
  echo ${array[$(jot -r 1 0 `expr ${#array[*]} - 1`)]}
}

# Generate a random food emoji.
function random_food () {
  echo $(random_el "🍺 🍸 🍹 🍷 🍕 🍔 🍟 🍗 🍖 🍝 🍤 🍣 🍥 🍜 🍡 🍞 🍩 🍦 🍨 🍰 🍪 🍫 🍬 🍭 🍎 🍏 🍊 🍋 🍒 🍇 🍉 🍓 🍑 🍌 🍐 🍍 🍆 🍅 🐶 🐱 🐭 🐹 🐰 🐻 🐼 🐨 🐯 🦁 🐮 🐷 🐸 🐵 🙈 🙉 🙊 🐔 🐧 🐦 🦊 🦄 🐝 🐙 🐠 🐡 🐳 ")
}

# Generate a random food emoji.
function random_hearts () {
  echo $(random_el "💙 🧡 💛 💚 💗")
}

# PS1, yeah it's a big deal!
red=$'\e[1;31;40m'
grn=$'\e[1;32;40m'
yel=$'\e[1;33;40m'
blu=$'\e[1;34;40m'
mag=$'\e[1;35;40m'
cyn=$'\e[1;36;40m'
end=$'\e[0m'
blink=$'\e[5;33;40m'

if [ -f /usr/share/git/completion/git-prompt.sh ]; then
         . /usr/share/git/completion/git-prompt.sh
fi

# Since the venv name written in PS1 by the virtualenv's postactivate
# script won't have colors, the original customized value is saved in
# the variable `ORIG_PS1` and re-exported in the postactivate script.
export ORIG_PS1="\[$red\]$(random_hearts)\[$end\] \[$blu\]\W \[$yel\]\$(__git_ps1 \"%s \")\[$grn\]$ \[$end\]"
export PS1=$ORIG_PS1

# Set tmux window status using food emoji as index.
function tmux_food () {
  if [[ "$TERM" = screen* ]] && [ -n "$TMUX" ]; then
    tmux_index=$(random_food)
    tmux set-window window-status-current-format " $tmux_index  #W "
    tmux set-window window-status-format " $tmux_index  #W "
  fi
}
tmux_food

# Load platform specific settings
platform=$(uname 2>/dev/null | tr '[:upper:]' '[:lower:]')
. $HOME/.bash.d/platform/$platform

# Setup programming framework specific settings
. $HOME/.bash.d/asdf

# Plugins and apps
# z- jump-list of the directories you actually use ( https://github.com/rupa/z )
. ~/bin/z.sh

# Android setup
PATH="$HOME/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
ANDROID_SDK_ROOT="$HOME/.android-tools"
ANDROID_HOME="$HOME/.android-tools"
JDK_HOME="$HOME/.sdkman/candidates/java/current"
PATH=$PATH:$ANDROID_HOME/emulator
PATH=$PATH:$ANDROID_HOME/tools
PATH=$PATH:$ANDROID_HOME/tools/bin
PATH=$PATH:$ANDROID_HOME/platform-tools
export PATH

# extract (https://github.com/alexrochas/zsh-extract)
. ~/bin/extract.plugin.zsh

# Instalacao das Funcoes ZZ (www.funcoeszz.net)
export ZZOFF=""  # desligue funcoes indesejadas
export ZZPATH="/usr/bin/funcoeszz"  # script
source "$ZZPATH"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/guerra/.sdkman"
[[ -s "/home/guerra/.sdkman/bin/sdkman-init.sh" ]] && source "/home/guerra/.sdkman/bin/sdkman-init.sh"
