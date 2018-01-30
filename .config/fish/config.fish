set fish_greeting ""

set -x EDITOR nvim

set -x GOPATH $HOME/dev
set -x GOBIN $GOPATH/bin

set -x NPM ~/.node_modules/bin
set -x NPMLOCAL node_modules/.bin
set -x npm_config_prefix ~/.node_modules

set -x PATH $NPMLOCAL $NPM $GOBIN $GAPP $PATH

alias g git
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
