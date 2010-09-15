#!/bin/zsh
##
# cloudhead - .zshrc
#

source ~/.profile

#
# Includes
#
autoload colors && colors
autoload -Uz vcs_info
autoload -U compinit && compinit
autoload -U complist


# Set input mode to vi
set -o vi

# Bind <C-r> to history search
bindkey "^r" history-incremental-search-backward
# History search with up and down arrow keys
bindkey '^[[A' history-beginning-search-backward
bindkey '^[[B' history-beginning-search-forward
bindkey -e

#
# Aliases
#
alias h='history'
alias l='ls -lhaAFG'
alias ll='l'

alias vi='vim'

alias grep='grep -i --color=auto'
alias hu='htop --sort-key cpu'
alias hm='htop --sort-key memory'
alias nr='rlwrap node-repl'

# git
alias g='git status -sb'
alias ga='git add'
alias gb='git branch'
alias gc='git commit -v'
alias gca='git commit -va'
alias gco='git checkout'
alias gcon='git branch --contains'
alias gd='git diff --no-ext-diff -w "$@" | vim -R -'
alias gdw='git diff --word-diff --no-ext-diff -w "$@" | vim -R -'
alias gf='git fetch'
alias gl='git log --no-ext-diff -w "$@" | vim -R -'
alias glo='git log --pretty=oneline --decorate -w "$@" | vim -R -'
alias glog='git log --pretty=oneline --decorate --graph --no-ext-diff -w "$@" | vim -R -'
alias gm='git mergetool -t opendiff'
alias gpl='git pull --rebase'
alias gps='git push'
alias gr='git rebase'
alias grev='git name-rev'
alias gri='git rebase -i'

function gg() {
  git commit -vam "$*"
}

# hg
alias hgd='hg diff | vim -R -'

# if [[ $(uname) = 'Darwin' ]]; then
#   alias l='ls -alAghp'
#   alias ls='ls -p'
# else
#   alias l='ls -alAGhp --color=always'
#   alias ls='ls -p --color=always'
# fi
alias ..='cd ..'

# other
alias nginxre='sudo kill `cat /usr/local/var/run/nginx.pid`;sudo nginx -c ~/development/dotfiles/nginx/nginx.conf'
alias ts='thin start'
alias src="cd $SRCPATH"
alias vi='vim'
alias e='vim'
alias wic='wicd-curses'
alias rcconf='sudo vim /etc/rc.conf'
alias chrome='google-chrome'

#
# History
#
HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000
REPORTTIME=10

setopt NO_BG_NICE
setopt NO_HUP
setopt NO_LIST_BEEP
setopt LOCAL_OPTIONS
setopt LOCAL_TRAPS
setopt EXTENDED_HISTORY
setopt PROMPT_SUBST
setopt CORRECT
setopt COMPLETE_IN_WORD
setopt IGNORE_EOF
setopt AUTO_CD
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY
setopt HIST_REDUCE_BLANKS
setopt extendedglob
setopt nomatch
setopt notify

unsetopt beep

#
# ANSI Color Codes
#
CLEAR=$reset_color
RED=$fg[red]
GREEN=$fg[green]
YELLOW=$fg[yellow]
YELLOW_BOLD=$fg_bold[yellow]
BLUE=$fg[blue]
PURPLE=$fg[magenta]
PURPLE_BOLD=$fg_bold[magenta]
CYAN=$fg[cyan]
CYAN_BOLD=$fg_bold[cyan]
# GREY=$'\e[0;94m'
GREY=$fg[grey]

#
# Set prompt style
#
# %b - branchname
# %u - unstagedstr (see below)
# %c - stangedstr (see below)
# %a - action (e.g. rebase-i)
# %R - repository path
# %S - path in the repository

ACTION="%{$PURPLE%}:%a%{$CLEAR%}"

zstyle ':vcs_info:*:prompt:*'                enable git
zstyle ':vcs_info:*:prompt:*'                check-for-changes true
zstyle ':vcs_info:*:prompt:*'  stagedstr     "%{$YELLOW%}"
zstyle ':vcs_info:*:prompt:*'  unstagedstr   "%{$GREEN%}"
zstyle ':vcs_info:*:prompt:*'  actionformats "(%u%c%b${ACTION}) "
zstyle ':vcs_info:*:prompt:*'  formats       "(%b%u) "
zstyle ':vcs_info:*:prompt:*'  nvcsformats   ""

function precmd {
  local last=$?
  local prompt=""

  # Status
  if [[ $last -eq 0 ]]; then
    prompt=$GREEN
  elif [[ $last  -eq 1 ]]; then
    prompt=$YELLOW
  else
    prompt=$RED
  fi

  cursor="%{$prompt%}%#%{$CLEAR%}"
  vcs_info 'prompt'
}

#
# Set Prompts
#
if [[ "$(hostname | grep -o -e "^alx-")" ==  "alx-" ]] then
  PROMPT="%{$CYAN%}%n%{$CLEAR%} %{$YELLOW%}%c%{$CLEAR%} "'${vcs_info_msg_0_}${cursor}'" %{$CLEAR%}"
else
  PROMPT="%{$PURPLE%}%n%{$CLEAR%}:%{$PURPLE%}%m%{$CLEAR%} %{$YELLOW_BOLD%}%c%{$CLEAR%} "'${vcs_info_msg_0_}${cursor}'" %{$CLEAR%}"
fi

RPROMPT='%{$CYAN%}%w %T%{$CLEAR%}'

#
# ls colors
#
LS_COLORS='rs=0:di=01;36:ln=00;35:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lz=01;31:*.xz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.rar=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.axv=01;35:*.anx=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.axa=00;36:*.oga=00;36:*.spx=00;36:*.xspf=00;36:';
ZLS_COLORS=$LS_COLORS
export LS_COLORS
export ZLS_COLORS
