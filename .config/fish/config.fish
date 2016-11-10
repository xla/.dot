set fish_greeting ""

set -x EDITOR vim

set -x GCLOUD $HOME/dev/google-cloud-sdk/bin
set -x GAPP $HOME/dev/go_appengine

set -x GOPATH $HOME/dev
set -x GOBIN $GOPATH/bin

set -x JAVA_HOME (/usr/libexec/java_home)

set -x PATH node_modules/.bin $GOBIN $GAPP $GCLOUD $PATH 

alias g git
alias l 'ls -lah'
alias vi vim

setenv SSH_ENV "$HOME/.ssh/environment"

if [ -n "$SSH_AGENT_PID" ]
  ps -ef | grep $SSH_AGENT_PID | grep ssh-agent > /dev/null
  if [ $status -eq 0 ]
    test_identities
  end 
else
  if [ -f $SSH_ENV ]
    . $SSH_ENV > /dev/null
  end
  ps -ef | grep $SSH_AGENT_PID | grep -v grep | grep ssh-agent > /dev/null
  if [ $status -eq 0 ]
    test_identities
  else
    start_agent
  end
end
