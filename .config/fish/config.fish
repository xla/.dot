set fish_greeting ""

set -x EDITOR nvim

set -x GPG_TTY (tty)

set -x GOPATH $HOME/dev
set -x GOBIN $GOPATH/bin

set -x CARGOBIN ~/.cargo/bin
set -x LOCALBIN ~/.local/bin
set -x NPM ~/.node_modules/bin
set -x NPMLOCAL node_modules/.bin
set -x npm_config_prefix ~/.node_modules

set -x PATH $LOCALBIN $NPMLOCAL $NPM $CARGOBIN $GOBIN $GAPP $PATH

set -x FZF_DEFAULT_COMMAND 'rg --files --no-ignore --hidden --follow --glob "!.git/*"'

switch (uname)
case Darwin
  set -g fish_user_paths "/usr/local/opt/node@8/bin" $fish_user_paths
end

alias g git
alias ga 'git add'
alias gl 'git pull'
alias gp 'git push'
alias l 'ls -lah'
alias pacman 'sudo pacman'
alias vi vim
alias vim nvim

setenv SSH_ENV "/tmp/ssh-environment"

if [ -n "$SSH_AGENT_PID" ]
  ps -ef | grep $SSH_AGENT_PID | grep ssh-agent > /dev/null
  if [ $status -eq 0 ]
    test_identities
  end
else
  if [ -f $SSH_ENV ]
    . $SSH_ENV > /dev/null
  end
  ps -ef | grep -v grep | grep ssh-agent > /dev/null
  if [ $status -eq 0 ]
    test_identities
  else
    start_agent
  end
end
