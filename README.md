# Dotfiles

macOS shell, editor and terminal configuration. Neovim uses minpac, CoC (not
native LSP), and Tree-sitter. The setup supports Apple Silicon and Intel Macs.

## Set up a Mac

Clone this repository somewhere permanent, then run as your normal user:

```sh
git clone https://github.com/xla/.dot.git ~/.dot # or use your existing checkout
cd ~/.dot
./setup-macos.sh --dry-run
./setup-macos.sh
```

The script uses its own location, so it works from any working directory and
does not require the name `~/.dot`.
Keep the checkout in place: your home configuration will be symlinked to it.

Options:

```sh
./setup-macos.sh --apps       # also install the GUI apps listed below
./setup-macos.sh --no-shell   # install everything, but don't change your login shell
./setup-macos.sh --links-only # link configs only; no downloads, installs or shell changes
```

`--dry-run` can be combined with any option and makes no changes. You can rerun
the setup after an interrupted installation or to update your tools and plugins.
An initial install needs internet access, disk space and time—especially for the
Rust cargo tools. Homebrew and `chsh` may request your password. **Do not run the
whole script with sudo.** If Xcode command-line tools are missing, the script
opens Apple's installer; finish it and rerun setup.

### What gets installed

- **Homebrew** if missing, plus everything in `Brewfile`: fish, Neovim, Git,
  ripgrep, **fzy** (required by `neovim-fuzzy`), fzf, GNU less, htop, LLVM,
  Tree-sitter CLI, Node/npm, Python, Go, golangci-lint, Terraform LS and LuaLS.
- **Pi coding agent and Herdr** through Homebrew (`pi-coding-agent` and `herdr`),
  included in the default installation without `--apps`. Setup checks that both
  commands are available in fish and that their version commands succeed.
- **Neovim's Python provider**: `pynvim` in `~/.venvs/neovim`. No packages are
  installed into the system Python. Neovim >= 0.12 is required by the current
  Tree-sitter plugin.
- **Rust through rustup**: stable, rust-src, rustfmt, clippy and rust-analyzer.
  A broken legacy `~/.cargo/bin/rustup` symlink is backed up before reinstalling.
- **Go tools in `~/.local/bin`**: gopls, goimports, gofumpt, staticcheck,
  govulncheck, dlv, gotests, gomodifytags, impl and gotestsum.
- **Cargo tools**: cargo-nextest, cargo-watch, cargo-expand, cargo-edit,
  cargo-audit, cargo-deny, cargo-outdated, bacon and taplo-cli.
- **All minpac plugins from `.vimrc`**, followed by all ten CoC extensions in
  `g:coc_global_extensions`. The script waits for asynchronous plugin installs,
  propagates failures, and preserves additional CoC extensions you installed.
- **All 18 configured Tree-sitter parsers**, including Go, Rust, web languages,
  Lua, Vim, Markdown and TOML. Parsers are installed/updated synchronously and
  checked for loadable libraries and valid highlight queries.

CoC uses the `gopls` and `rust-analyzer` on your PATH, not machine-specific paths
or privately downloaded copies. LuaLS is installed by Homebrew and started via
CoC's `languageserver.lua` configuration; `coc-lua` is retained for its settings
schema, with its private downloader disabled. Project ESLint/Prettier configs
and dependencies still belong in each project's `package.json`. In particular,
Prettier only formats projects with a config (`prettier.requireConfig`).

`Brewfile.apps` is optional and installs only **Ghostty, Dropbox, BetterDisplay,
ChatGPT, Zen Browser, Brave Browser and Firefox**. Existing app configurations
are still linked even without `--apps`; kitty, Alacritty, Karabiner and VS Code
are not installed by setup. If you install Karabiner separately, its
input-monitoring/accessibility permissions must be approved in macOS.
The terminal configs use **PragmataPro**, a commercial font: install your licensed
copy separately, or change the configured font. It is not downloaded by setup.

### Links and backups

Setup links only the relevant, checked-in macOS configs:

- `~/.bash_profile`, `~/.gitconfig`, `~/.dircolors`, `~/.vimrc`, and `~/.vim`.
- `~/.config/nvim` directly to the checkout's `.vim` directory. The shared
  `init.vim` points to `.vimrc`; Vim and Neovim share settings and plugin files.
- The tracked fish config, prompt/title functions and rustup startup file.
- kitty, Ghostty, Alacritty (current TOML and legacy YAML), htop and Karabiner.
- `~/.cargo/config.toml` (its Linux-only target section has no effect on macOS).
- VS Code settings in `~/Library/Application Support/Code/User/settings.json`.

Existing files, directories and broken symlinks at those destinations are moved
to `~/.dotfiles-backups/<timestamp>-<pid>/`, preserving their relative paths.
Correct links are left alone on subsequent runs. To undo a link, remove that
symlink and move the corresponding backup back to its original location.
Replacing an existing `~/.vim` or Neovim config directory backs up that entire
directory, including any local plugins/settings.

The script does **not** replace all of `~/.config` or `~/.cargo`: local state,
credentials, fish universal variables and untracked configs are left alone.
The shared `.vim/pack` plugin checkouts and `.vim/tmp` state live in this checkout
and are Git-ignored. `~/.gitconfig` contains this repository's Git identity;
review it if setting up a machine for someone else.

Fish gets architecture-appropriate Homebrew paths, `~/.local/bin`,
`~/.cargo/bin`, and `~/.node_modules/bin`. `GOBIN` is `~/.local/bin`, and `GOPATH`
is `~/.local/share/go`. Startup preserves macOS's launchd SSH agent rather than
creating one or loading a machine-specific key. Add your own keys explicitly:

```sh
ssh-add --apple-use-keychain ~/.ssh/<your-key>
```

Unless `--no-shell` is used, setup registers Homebrew fish in `/etc/shells` and
sets it as your login shell. Open a new terminal after installation. The existing
`.macos` preferences script is **not executed** (it includes broad system and
security-related changes). Linux/X11/systemd/browser-profile files are not linked.

## Pi and Herdr

After setup, run `pi` in a project directory. Use `/login` inside Pi to connect
your model provider; setup does not configure credentials. Start Herdr with
`herdr`. Existing Pi credentials/sessions in `~/.pi/agent` and Herdr's local
configuration/session state in `~/.config/herdr` are left untouched.

Both tools are managed by Homebrew; rerunning setup updates them along with the
other Homebrew packages. To update only these tools, use:

```sh
brew upgrade pi-coding-agent herdr
```

## Verify and maintain Neovim

After setup, open Neovim and check:

```vim
:checkhealth coc nvim-treesitter vim.treesitter vim.provider
:CocInfo
:CocList extensions
:CocList services
```

Open a Go file in a Go module and a Rust file in a Cargo workspace to check that
`gopls` and `rust-analyzer` start. For Lua, check the `lua` language service.

For manual maintenance:

```vim
:PackUpdate
:PackStatus
:CocUpdate
:CocRestart
:TSUpdate
```

Install a missing parser with `:TSInstall <language>`, or rerun setup to install
all configured parsers/extensions. The single source of truth for both lists is
`.vimrc`, also used through `.vim/init.vim`.

Rust and Go tool updates can be performed by rerunning setup. `cargo expand`
can require a nightly toolchain for some projects; stable remains the default.
Optional nightly support can be added with `rustup toolchain install nightly`.

## Offline setup tests

```sh
bash -n setup-macos.sh
fish --no-execute .config/fish/config.fish
python3 -m unittest discover -s tests -v
```

Tests use temporary home directories and check backups, idempotency, broken
symlinks, a pre-existing `~/.config` symlink, checkout paths containing spaces,
dry-run safety and refusal to move a directory containing the checkout. When
fish/Neovim are available, they also check clean-home shell startup and Neovim's
first-call minpac initialization. They do not download anything or install/change
tools in your actual home directory.
