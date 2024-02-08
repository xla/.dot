set fish_greeting ""

set -x EDITOR nvim

set -x XDG_CONFIG_HOME  $HOME/.config
set -x XDG_BIN_HOME     $HOME/.local/bin
set -x XDG_DATA_HOME    $HOME/.local/share

set -x GNUPGHOME  $HOME/.gnupg

set -x npm_config_prefix  $HOME/.node_modules

set -x CARGOBIN $HOME/.cargo/bin
set -x CUDABIN  /opt/cuda/bin
set -x GEMBIN   $HOME/.local/share/gem/ruby/2.7.0/bin
set -x GOBIN    $XDG_BIN_HOME
set -x GOPATH   $HOME
set -x NPM      $HOME/.node_modules/bin
set -x NPMLOCAL node_modules/.bin
set -x RADBIN   $HOME/.radicle/bin

set -x PATH $XDG_BIN_HOME $CARGOBIN $CUDABIN $GEMBIN $GOBIN $NPM $NPMLOCAL $RADBIN $PATH

set -x FZF_DEFAULT_COMMAND 'rg --files --no-ignore --hidden --follow --glob "!.git/*"'
set -x LESS '-asrRix8'

switch (uname)
case Darwin
  set -g fish_user_paths "/usr/local/opt/node@8/bin" $fish_user_paths
end

alias g git
alias ga 'git add'
alias gl 'git pull'
alias gp 'git push'
alias l 'ls -lah'
alias vi vim
alias vim nvim

set -x GPG_TTY (tty)
set -x SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
gpgconf --launch gpg-agent
echo UPDATESTARTUPTTY | gpg-connect-agent -q > /dev/null

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
