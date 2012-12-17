# history
HISTFILE=~/.zsh_history
HISTSIZE=5000
SAVEHIST=1000

autoload -U colors && colors
autoload -U bashcomp
autoload -U compinit && compinit
autoload -U run-help
autoload -Uz vcs_info

bindkey -e

setopt autocd extendedglob prompt_subst
setopt appendhistory share_history histignorealldups

# default apps
(( ${+PAGER} )) || export PAGER="less"

# prompt
precmd() {
  vcs_info
}

zstyle ':vcs_info:*' formats "%b"

local return_code="%(?..%{$fg[red]%})"
export PS1='%c ${return_code}➤%{$reset_color%} '
export RPS1='${vcs_info_msg_0_}'

# colorful listings
zmodload -i zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# aliases
alias mv='nocorrect mv'       # no spelling correction on mv
alias cp='nocorrect cp'
alias mkdir='nocorrect mkdir'
alias j=jobs
if ls -F --color=auto >&/dev/null; then
  alias ls="ls --color=auto -F"
else
  alias ls="ls -F"
fi
alias l="ls -lah"
alias ll="ls -l"
alias l.='ls -d .[^.]*'
alias lsd='ls -ld *(-/DN)'
alias md='mkdir -p'
alias rd='rmdir'
alias cd..='cd ..'
alias ..='cd ..'
alias po='popd'
alias pu='pushd'
alias tsl="tail -f /var/log/syslog"
alias df="df -hT"
alias g="git"

# functions
setenv() { export $1=$2 }  # csh compatibility
sdate() { date +%Y.%m.%d }

source ~/.profile
