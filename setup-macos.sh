#!/bin/bash
# Run as your normal user. Compatible with macOS's system Bash 3.2.
set -euo pipefail
trap 'printf "\nSetup failed at line %s. Fix the error above and rerun.\n" "$LINENO" >&2' ERR

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
DRY_RUN=0
LINKS_ONLY=0
APPS=0
CHANGE_SHELL=1
BACKUP_ROOT="$HOME/.dotfiles-backups/$(date +%Y%m%d-%H%M%S)-$$"

usage() {
    printf '%s\n' \
        'Usage: ./setup-macos.sh [--dry-run] [--links-only] [--apps] [--no-shell]' \
        '' \
        '  --dry-run     Print actions without changing anything (works on any OS).' \
        '  --links-only  Only link configs; do not install tools or change the shell.' \
        '  --apps        Install Ghostty, Dropbox, BetterDisplay, ChatGPT, Zen, Brave and Firefox.' \
        '  --no-shell    Do not register fish or change your login shell.' \
        '' \
        'Existing configs are backed up under ~/.dotfiles-backups before linking.'
}

for arg in "$@"; do
    case "$arg" in
        --dry-run) DRY_RUN=1 ;;
        --links-only) LINKS_ONLY=1 ;;
        --apps) APPS=1 ;;
        --no-shell) CHANGE_SHELL=0 ;;
        --help|-h) usage; exit 0 ;;
        *) usage >&2; exit 2 ;;
    esac
done

log() { printf '\n==> %s\n' "$*"; }
run() {
    printf '+'
    printf ' %q' "$@"
    printf '\n'
    if [ "$DRY_RUN" -eq 0 ]; then "$@"; fi
}

# Back up files, directories and broken symlinks without deleting their contents.
backup() {
    local target="$1" relative="${1#"$HOME"/}"
    if [ -e "$target" ] || [ -L "$target" ]; then
        run mkdir -p "$(dirname "$BACKUP_ROOT/$relative")"
        run mv "$target" "$BACKUP_ROOT/$relative"
    fi
}
link() {
    local source="$1" target="$2"
    if [ ! -e "$source" ]; then
        printf 'Missing source: %s\n' "$source" >&2
        exit 1
    fi
    # Also handles an existing ~/.config symlink into this repository.
    if [ "$source" -ef "$target" ]; then
        printf 'Already linked: %s\n' "$target"
        return
    fi
    # Moving an ancestor of the checkout would break the repository itself.
    if [ -d "$target" ] && [ ! -L "$target" ]; then
        case "$ROOT/" in
            "$(cd "$target" && pwd -P)/"*)
                printf 'Refusing to replace directory containing the checkout: %s\n' "$target" >&2
                exit 1 ;;
        esac
    fi
    backup "$target"
    run mkdir -p "$(dirname "$target")"
    run ln -s "$source" "$target"
}

if [ -z "${HOME:-}" ] || [ "$HOME" = / ]; then
    printf 'HOME must point to your home directory.\n' >&2
    exit 1
fi
if [ "$DRY_RUN" -eq 0 ]; then
    if [ "$(uname -s)" != Darwin ]; then
        printf 'This setup requires macOS. Use --dry-run to inspect it elsewhere.\n' >&2
        exit 1
    fi
    if [ "$(id -u)" -eq 0 ]; then
        printf 'Run as your normal user, not with sudo.\n' >&2
        exit 1
    fi
fi

# Deliberately use the standard paths that fish exports, not inherited XDG,
# GOBIN or NVIM_APPNAME overrides from the invoking shell.
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"
export GOBIN="$HOME/.local/bin"
export GOPATH="$HOME/.local/share/go"
export CARGO_HOME="$HOME/.cargo"
export RUSTUP_HOME="$HOME/.rustup"
export npm_config_prefix="$HOME/.node_modules"
unset NVIM_APPNAME VIMINIT EXINIT

if [ "$LINKS_ONLY" -eq 0 ]; then
    log 'Xcode command-line tools'
    if [ "$DRY_RUN" -eq 1 ]; then
        printf 'Would request Xcode command-line tools if missing.\n'
    elif ! xcode-select -p >/dev/null 2>&1; then
        xcode-select --install
        printf 'Finish the command-line tools installer, then rerun this script.\n'
        exit 1
    fi

    log 'Homebrew'
    BREW="$(command -v brew || true)"
    if [ -z "$BREW" ]; then
        if [ "$(uname -m)" = arm64 ]; then BREW=/opt/homebrew/bin/brew; else BREW=/usr/local/bin/brew; fi
    fi
    if [ ! -x "$BREW" ]; then
        if [ "$DRY_RUN" -eq 1 ]; then
            printf 'Would download and run the official Homebrew installer.\n'
        else
            installer="$(mktemp)"
            curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh -o "$installer"
            /bin/bash "$installer"
            rm -f "$installer"
        fi
    fi
    if [ "$DRY_RUN" -eq 0 ]; then
        eval "$("$BREW" shellenv)"
        BREW_PREFIX="$("$BREW" --prefix)"
    else
        BREW_PREFIX="${BREW%/bin/brew}"
    fi
    export PATH="$HOME/.local/bin:$HOME/.cargo/bin:$BREW_PREFIX/bin:$BREW_PREFIX/sbin:$BREW_PREFIX/opt/llvm/bin:$PATH"
    run "$BREW" bundle --file="$ROOT/Brewfile"
    if [ "$APPS" -eq 1 ]; then run "$BREW" bundle --file="$ROOT/Brewfile.apps"; fi
fi

log 'Linking macOS configs (leaving untracked files and Linux configs alone)'
for file in .bash_profile .gitconfig .dircolors .vimrc; do
    link "$ROOT/$file" "$HOME/$file"
done
link "$ROOT/.vim" "$HOME/.vim"
# The repository's .config/nvim -> ../.vim is NOT valid when linked directly.
link "$ROOT/.vim" "$HOME/.config/nvim"
for file in \
    fish/config.fish fish/conf.d/rustup.fish \
    fish/functions/fish_prompt.fish fish/functions/fish_title.fish \
    kitty/kitty.conf ghostty/config.ghostty \
    alacritty/alacritty.yml alacritty/alacritty.toml \
    htop/htoprc karabiner/karabiner.json \
    karabiner/assets/complex_modifications/1542039256.json; do
    link "$ROOT/.config/$file" "$HOME/.config/$file"
done
link "$ROOT/.cargo/config.toml" "$HOME/.cargo/config.toml"
# VS Code uses Application Support rather than ~/.config/Code on macOS.
link "$ROOT/.config/Code/User/settings.json" "$HOME/Library/Application Support/Code/User/settings.json"

if [ "$LINKS_ONLY" -eq 1 ]; then
    log "Configs linked. Backups (if needed): $BACKUP_ROOT"
    exit 0
fi

log 'Neovim Python provider'
run mkdir -p "$HOME/.local/bin" "$HOME/.vim/tmp/swap" "$HOME/.vim/tmp/undo"
run "$BREW_PREFIX/bin/python3" -m venv "$HOME/.venvs/neovim"
run "$HOME/.venvs/neovim/bin/python" -m pip install --upgrade pip pynvim

log 'Rust stable toolchain'
if [ ! -x "$HOME/.cargo/bin/rustup" ]; then
    # This also handles a stale Homebrew rustup-init symlink from older setups.
    backup "$HOME/.cargo/bin/rustup"
    if [ "$DRY_RUN" -eq 1 ]; then
        printf 'Would run https://sh.rustup.rs with -y --no-modify-path --default-toolchain stable.\n'
    else
        installer="$(mktemp)"
        curl --proto '=https' --tlsv1.2 -fsSL https://sh.rustup.rs -o "$installer"
        RUSTUP_INIT_SKIP_PATH_CHECK=yes /bin/sh "$installer" -y --no-modify-path --default-toolchain stable
        rm -f "$installer"
    fi
fi
run "$HOME/.cargo/bin/rustup" toolchain install stable
run "$HOME/.cargo/bin/rustup" default stable
run "$HOME/.cargo/bin/rustup" component add rust-src rustfmt clippy rust-analyzer

log 'Go development tools'
for tool in \
    golang.org/x/tools/gopls \
    golang.org/x/tools/cmd/goimports \
    mvdan.cc/gofumpt \
    honnef.co/go/tools/cmd/staticcheck \
    golang.org/x/vuln/cmd/govulncheck \
    github.com/go-delve/delve/cmd/dlv \
    github.com/cweill/gotests/gotests \
    github.com/fatih/gomodifytags \
    github.com/josharian/impl \
    gotest.tools/gotestsum; do
    run "$BREW_PREFIX/bin/go" install "$tool@latest"
done

log 'Rust development tools (initial compilation can take a while)'
for tool in cargo-nextest cargo-watch cargo-expand cargo-edit cargo-audit cargo-deny cargo-outdated bacon taplo-cli; do
    run "$HOME/.cargo/bin/cargo" install "$tool" --locked
done

log 'Neovim plugins, CoC extensions and Tree-sitter parsers'
MINPAC="$HOME/.vim/pack/minpac/opt/minpac"
if [ ! -f "$MINPAC/autoload/minpac.vim" ]; then
    if [ -e "$MINPAC" ] || [ -L "$MINPAC" ]; then
        printf 'Incomplete minpac checkout at %s; move it aside and rerun.\n' "$MINPAC" >&2
        exit 1
    fi
    run git clone --depth 1 https://github.com/k-takata/minpac.git "$MINPAC"
fi
export DOTFILES_ROOT="$ROOT"
# Separate processes: newly installed plugins only load on the second startup.
for stage in plugins editor; do
    run env DOTFILES_NVIM_STAGE="$stage" "$BREW_PREFIX/bin/nvim" --headless \
        --cmd 'let g:dotfiles_bootstrap = 1 | let g:coc_start_at_startup = 0' \
        -c 'lua dofile(vim.env.DOTFILES_ROOT .. "/scripts/bootstrap-nvim.lua")'
done

log 'Checking fish and toolchain startup'
run "$BREW_PREFIX/bin/fish" --no-execute "$ROOT/.config/fish/config.fish"
run "$BREW_PREFIX/bin/fish" -c 'for tool in pi herdr nvim node npm python3 rg fzy fzf tree-sitter terraform-ls lua-language-server gopls goimports gofumpt staticcheck govulncheck dlv gotests gomodifytags impl gotestsum golangci-lint cargo rustc rust-analyzer rustfmt bacon taplo; command -q $tool; or exit 1; end'
run "$HOME/.venvs/neovim/bin/python" -c 'import pynvim'
run "$HOME/.cargo/bin/rust-analyzer" --version
run "$HOME/.local/bin/gopls" version
run "$BREW_PREFIX/bin/pi" --version
run "$BREW_PREFIX/bin/herdr" --version

if [ "$CHANGE_SHELL" -eq 1 ]; then
    log 'Registering fish as the login shell (may ask for your password)'
    FISH="$BREW_PREFIX/bin/fish"
    if ! grep -Fxq "$FISH" /etc/shells; then
        if [ "$DRY_RUN" -eq 1 ]; then
            printf 'Would append %s to /etc/shells using sudo.\n' "$FISH"
        else
            printf '%s\n' "$FISH" | sudo tee -a /etc/shells >/dev/null
        fi
    fi
    if [ "$DRY_RUN" -eq 1 ]; then
        run chsh -s "$FISH"
    else
        current_shell="$(dscl . -read "/Users/$(id -un)" UserShell | awk '{print $2}')"
        if [ "$current_shell" != "$FISH" ]; then run chsh -s "$FISH"; fi
    fi
fi

log 'Setup complete. Open a new terminal, then run nvim.'
printf 'Backups (if needed): %s\n' "$BACKUP_ROOT"
printf 'PragmataPro is a commercial font; install your licensed copy separately.\n'
printf 'System preferences in .macos were NOT applied.\n'
