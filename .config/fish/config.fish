set fish_greeting ""

set -gx EDITOR nvim
set -gx VISUAL nvim

set -x XDG_CONFIG_HOME $HOME/.config
set -x XDG_BIN_HOME $HOME/.local/bin
set -x XDG_DATA_HOME $HOME/.local/share

set -x npm_config_prefix $HOME/.node_modules

set -x CARGOBIN $HOME/.cargo/bin
set -x GEMBIN $HOME/.local/share/gem/ruby/2.7.0/bin
set -x GOBIN $XDG_BIN_HOME
set -x GOPATH $HOME
set -x HOME_NODE_MODULES_BIN $HOME/.node_modules/bin
set -x NPMLOCAL node_modules/.bin

set -x PATH $XDG_BIN_HOME $CARGOBIN $GEMBIN $GOBIN $HOME_NODE_MODULES_BIN $NPMLOCAL $PATH

fish_add_path /opt/homebrew/bin
fish_add_path /opt/homebrew/opt/llvm/bin
set -gx LDFLAGS "-L/opt/homebrew/opt/llvm/lib"
set -gx CPPFLAGS "-I/opt/homebrew/opt/llvm/include"

set -x FZF_DEFAULT_COMMAND 'rg --files --no-ignore --hidden --follow --glob "!.git/*"'
set -x LESS '--use-color -R'

switch (uname)
case Darwin
    set -g fish_user_paths "/usr/local/opt/node@8/bin" $fish_user_paths
end

alias g  'git'
alias ga 'git add'
alias gl 'git pull'
alias gp 'git push'
alias l  'ls -lah'
alias vi vim
alias vim nvim

setenv SSH_ENV "/tmp/ssh-environment"

function start_agent
    ssh-agent -c | sed 's/^echo/#echo/' > $SSH_ENV
    chmod 600 $SSH_ENV
    . $SSH_ENV >/dev/null
    ssh-add ~/.ssh/id_xla-macmini.local
end

function test_identities
    ssh-add -l | grep "The agent has no identities" >/dev/null
    if test $status -eq 0
        ssh-add ~/.ssh/id_xla-macmini.local 2>/dev/null
        if test $status -eq 2
            start_agent
        end
    end
end

if test -n "$SSH_AGENT_PID"
    ps -ef | grep $SSH_AGENT_PID | grep ssh-agent >/dev/null
    if test $status -eq 0
        test_identities
    end
else
    if test -f $SSH_ENV
        . $SSH_ENV >/dev/null
    end
    ps -ef | grep -v grep | grep ssh-agent >/dev/null
    if test $status -eq 0
        test_identities
    else
        start_agent
    end
end

# Hermes Agent command
fish_add_path "$HOME/.local/bin"
