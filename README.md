# Dotfiles setup notes

This repository contains my shell and editor configuration. Most editor language
support is configured in `.vimrc` / `.vim/init.vim` and `.vim/coc-settings.json`.
The Neovim setup uses CoC rather than native LSP.

## Neovim plugins and CoC extensions

Install/update Vim plugins from Neovim:

```vim
:PackUpdate
```

Install the CoC extensions expected by `.vim/coc-settings.json`:

```vim
:CocInstall coc-json coc-eslint coc-prettier coc-yaml coc-tsserver coc-svelte coc-lua coc-rust-analyzer coc-go coc-snippets
```

After changing CoC settings or installing extensions:

```vim
:CocRestart
```

## Tree-sitter parsers

Install parsers used by the editor config:

```vim
:TSInstall go gomod gosum gowork rust toml
```

Or update all configured parsers:

```vim
:TSUpdate
```

## Go development tools

The Go setup expects tools to be installed explicitly into `$GOBIN`, which is
configured as `~/.local/bin` and should be on `PATH`.

```sh
go install golang.org/x/tools/gopls@latest
go install golang.org/x/tools/cmd/goimports@latest
go install mvdan.cc/gofumpt@latest
go install honnef.co/go/tools/cmd/staticcheck@latest
go install golang.org/x/vuln/cmd/govulncheck@latest
go install github.com/go-delve/delve/cmd/dlv@latest
go install github.com/cweill/gotests/gotests@latest
go install github.com/fatih/gomodifytags@latest
go install github.com/josharian/impl@latest
go install gotest.tools/gotestsum@latest
brew install golangci-lint
```

Verify:

```sh
go version
gopls version
goimports --help >/dev/null
gofumpt -version
staticcheck -version
govulncheck -version
dlv version
gotests -version
gomodifytags --help >/dev/null
impl --help >/dev/null
gotestsum --version
golangci-lint version
```

The CoC Go configuration points directly at the user-managed `gopls`:

```jsonc
"go.goplsPath": "/Users/xla/.local/bin/gopls",
"go.checkForUpdates": "disabled"
```

## Rust toolchain

Install Rust through `rustup` and ensure the standard developer components are
present:

```sh
rustup toolchain install stable
rustup default stable
rustup component add rust-src rustfmt clippy rust-analyzer
```

If using Homebrew `rustup`, verify that the cargo shims are not pointing at a
stale `rustup-init` path. The expected commands are:

```sh
cargo --version
rustc --version
rust-analyzer --version
rustfmt --version
cargo clippy --version
```

If `~/.cargo/bin/rustup` is a broken symlink to
`/opt/homebrew/bin/rustup-init`, repair it with:

```sh
ln -sf /opt/homebrew/opt/rustup/libexec/bin/rustup ~/.cargo/bin/rustup
```

Then restart the shell or refresh command lookup:

```sh
hash -r # bash/zsh
# or: exec fish
```

## Rust development tools

Useful cargo tools for serious Rust development:

```sh
cargo install cargo-nextest --locked
cargo install cargo-watch --locked
cargo install cargo-expand --locked
cargo install cargo-edit --locked
cargo install cargo-audit --locked
cargo install cargo-deny --locked
cargo install cargo-outdated --locked
cargo install bacon --locked
cargo install taplo-cli --locked
```

Verify:

```sh
cargo nextest --version
cargo watch --version
cargo expand --version
cargo audit --version
cargo deny --version
cargo outdated --version
bacon --version
taplo --version
```

## Neovim health checks

Useful checks after bootstrapping a machine:

```vim
:checkhealth coc nvim-treesitter vim.treesitter vim.provider
:CocInfo
:CocList extensions
:CocList services
```

For Go, open a file in a Go module and confirm `gopls` starts. For Rust, open a
file in a Cargo workspace and confirm `rust-analyzer` starts.
