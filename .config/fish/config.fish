set fish_greeting ""

set -gx EDITOR nvim
set -gx VISUAL nvim

set -gx XDG_CONFIG_HOME $HOME/.config
set -gx XDG_BIN_HOME $HOME/.local/bin
set -gx XDG_DATA_HOME $HOME/.local/share

set -gx npm_config_prefix $HOME/.node_modules
set -gx CARGOBIN $HOME/.cargo/bin
set -gx GOBIN $XDG_BIN_HOME
set -gx GOPATH $HOME/.local/share/go

# GUI terminals don't necessarily inherit Homebrew's PATH. Support both Apple
# Silicon and Intel installations, including Homebrew's share/man directories.
if test (uname) = Darwin
    for brew_prefix in /opt/homebrew /usr/local
        if test -x $brew_prefix/bin/brew
            eval ($brew_prefix/bin/brew shellenv)
            fish_add_path --global --move $brew_prefix/opt/llvm/bin $brew_prefix/opt/less/bin
            set -gx LDFLAGS "-L$brew_prefix/opt/llvm/lib"
            set -gx CPPFLAGS "-I$brew_prefix/opt/llvm/include"
            break
        end
    end
end

# Global (not universal) paths avoid accumulating duplicate entries on restarts.
fish_add_path --global --move $XDG_BIN_HOME $CARGOBIN $HOME/.node_modules/bin

set -gx FZF_DEFAULT_COMMAND 'rg --files --no-ignore --hidden --follow --glob "!.git/*"'
set -gx LESS '--use-color -R'

alias g 'git'
alias ga 'git add'
alias gl 'git pull'
alias gp 'git push'
alias l 'ls -lah'
alias vi nvim
alias vim nvim

# macOS supplies an SSH agent through launchd. Keep its SSH_AUTH_SOCK; don't
# start another agent or try to load a private key specific to one machine.
# Add your own keys with: ssh-add --apple-use-keychain ~/.ssh/<key>
