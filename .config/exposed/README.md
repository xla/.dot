# Exposed — the light counterpart to Shady

**Exposed** keeps Shady's restrained hierarchy, but makes every text role
readable on light surfaces: white paper, neutral gray chrome, dark gray syntax,
muted teal types/identifiers, green numbers/additions, and maroon errors/removals.
Warnings use dark olive rather than unreadable pale yellow.

All four palettes are called **exposed**:

| Tool | Theme artifact | Automatic light/dark selection |
| --- | --- | --- |
| Neovim | `.vim/colors/exposed.vim` | Native `background` detection/reload → Exposed / Shady |
| Ghostty | `.config/ghostty/themes/exposed` | `theme = light:exposed,dark:shady` |
| Pi | `.pi/agent/themes/exposed.json` (`name: exposed`) | `theme: exposed/shady` (light first) |
| Herdr | `.config/herdr/themes/exposed.toml` | Native `[theme.custom.light]` / `[theme.custom.dark]` |

**Herdr naming limitation:** 0.9.3 only accepts built-in theme names. It cannot
register an `exposed` selector entry. The Exposed file is the named custom
palette; its 19 colors are generated into the supported light override table.
`light_name = "one-light"` and `dark_name = "vesper"` are internal bases only,
with every color overridden. Setting either name to `exposed` would cause a
warning and fallback, not install the requested theme. No unsupported key or
background switching daemon is used.

## Appearance switching

Ghostty follows the desktop's **Light / Dark / Auto** appearance. Herdr,
Neovim (0.11+), and Pi receive the terminal's light/dark notifications and
reported colors. This keeps nested sessions aligned with their host, including
over SSH when the terminal path forwards the protocol. There is no periodic
`defaults` subprocess or macOS-only timer in the editor.

- Ghostty: native light/dark theme pair, `window-theme = auto`.
- Herdr: `auto_switch = true`, complete light and dark custom tables.
- Pi: save `"theme": "exposed/shady"` in **user** settings, preserving other
  preferences. Themes themselves have explicit `appearance` metadata.
- Neovim: local theme entrypoints honor `background` rather than forcing the
  old appearance back when Neovim reloads on a terminal update. The upstream
  Shady plugin is not edited. CoC overrides belong to the theme, not late,
  dark-only `.vimrc` commands. Embedded terminal ANSI palettes switch too.

The installed config files already link into this checkout. Herdr and Ghostty
were live-reloaded without changing system preferences or taking focus. New
Pi sessions use the automatic pair. **Run `/reload` in an existing Pi session**
to load the newly added theme and saved automatic selection. Reload `.vimrc`
or restart an existing Neovim session once to load its new entrypoints.

If appearance notifications are stripped by another multiplexer or an old
terminal, live switching cannot be guaranteed by a color file. Use a supported
terminal path; Pi falls back to `COLORFGBG`, then dark if no appearance is
reported. Neovim GUIs must report their background. Headless Neovim has no
system terminal to query; tests set `background` explicitly.

For explicit preview (not automatic mode):

```sh
nvim --cmd 'set background=light' '+colorscheme exposed'
pi --use-theme exposed
```

Preview Exposed in a light terminal: Pi's unstyled text and the pane canvas
still use the host terminal defaults.

## Contrast contract

The guarantee is **test-enforced contrast for theme-controlled text on the
configured opaque surfaces**, not a promise about arbitrary application output,
future plugins, emoji/images, visual impairments, or a terminal whose palette
or opacity has been overridden.

- Every Exposed Pi foreground role, including comments, muted hints, thinking
  text, fences, and border glyphs: **≥4.5:1** on all six configured surfaces.
  The minimum is **4.81:1**, with 576 pairs checked across truecolor and 256color.
- Herdr's complete light palette: all foreground/border tokens checked against
  six surfaces (78 combinations), minimum **4.81:1**. Native status symbols and
  text remain enabled; warning/attention/completion do not depend on color alone.
- Neovim: all resolved, visible highlights in the installed configuration are
  audited in GUI RGB and cterm, after light → dark → light reload. This includes
  syntax, Tree-sitter/semantic tokens, diagnostics, popup selection, searching,
  diffs, Markdown, CoC floats/lists/hints, line numbers, and separators.
  Underlines/undercurls also meet **3:1**. CoC's deliberately invisible cursor
  utility is excluded because it renders no text.
- All 16 Exposed ANSI foregrounds, cursor text, and selection text are checked.
- Ghostty uses **`minimum-contrast = 1`** to preserve exact palette and application
  colors, matching Kitty rather than recoloring Shady's darker colors. Backgrounds
  remain opaque; unfocused panes are not faded. Arbitrary terminal text is not
  guaranteed to meet Exposed's audited contrast floor.

The palette uses exact **extended xterm indices (16–255)** with corresponding
RGB colors, so light-mode 256color conversion cannot silently lower contrast.
The ANSI palette's “white” entries are intentionally dark grays on light paper.
CoC's built-in fixed Gruvbox ANSI matrix is replaced with readable foregrounds
on light tints, including otherwise invisible same-color pairs. Arbitrary new
plugin colors are not silently recolored: rerun the live-highlight audit after
plugin updates. The original Shady source is retained, not falsely claimed to
meet Exposed's text contrast floor.

### Palette

| Role | Exposed color |
| --- | --- |
| Paper / panels / selection | `#ffffff` / `#eeeeee` / `#dadada` |
| Body / keywords / headings | `#303030` / `#1c1c1c` / `#080808` |
| Comments / secondary syntax | `#585858` / `#444444` |
| Types / identifiers | `#005f5f` |
| Numbers / success / additions | `#005f00` |
| Errors / attention / removals | `#87005f` |
| Failed-tool surface | `#ffd7d7` |
| Warning / working | `#5f5f00` |
| Links / notifications / completion | `#005f87` |

## Source of truth and validation

Edit **`palette.json`**, then regenerate. Do not hand-edit generated themes.
`generate.py` also expands the upstream Shady language/plugin aliases and merges
only the Herdr theme region, preserving unrelated settings. The dark Herdr
palette is kept in `../herdr/themes/shady.toml`.

```sh
python3 .config/exposed/generate.py
python3 .config/exposed/generate.py --check
python3 .config/exposed/validate.py
PI_PACKAGE="$(brew --prefix pi-coding-agent)/libexec/lib/node_modules/@earendil-works/pi-coding-agent"
PI_OFFLINE=1 node .pi/scripts/validate-exposed.mjs "$PI_PACKAGE" --installed
PI_OFFLINE=1 node .pi/scripts/validate-shady.mjs "$PI_PACKAGE" --installed --nvim
python3 .config/herdr/tests/validate-shady.py
```

The validators use the real Ghostty/Herdr config parsers, installed Pi
schema/renderer/resource discovery/export, and the actual Neovim configuration.
Negative controls reject missing Pi roles and pale text on white. The generation
check catches palette drift between tools. `--preview` prints an actual Pi
Markdown render. Tested versions: **Neovim 0.12.5, Ghostty 1.3.1, Pi 1.0.4,
Herdr client/server 0.9.3**. Version/API changes need a reviewed audit update.

### Native live switching regression

```sh
python3 .config/exposed/terminal-switching.py
```

This exercises real Neovim and Pi TUIs in isolated PTYs, answers terminal color
queries, and sends light → dark → light Mode 2031 notifications. It verifies
automatic Exposed → Shady → Exposed, not just a theme-name resolver or a manual
`colorscheme` command. No model is called, system setting is changed, or input
sent to the user's actual terminal. Run it as a background task in Pi.

### Actual Pi-in-Herdr session

Use the [existing isolated fixture setup](../herdr/README.md#repeatable-real-pi-in-herdr-session),
but link **both** user theme files into the temporary agent directory, set its
`theme` to **`exposed/shady`**, and add `THEME_SESSION_THEME=exposed` to the pane
command. **Omit `--use-theme shady`** so this test exercises native automatic
selection. Run while the host is in light mode. The fixture is explicitly loaded,
uses a deterministic offline provider, and only edits its guarded temporary file.

After its `completed.json` appears:

```sh
python3 .config/exposed/validate.py --run-dir "$RUN"
```

The completed live test automatically selected Exposed and exercised a real
read, edit/diff, deliberate failed tool, Markdown/code/link, and simulated
attention. Actual VT colors were read back through Herdr; official Pi integration
reported idle → working → simulated blocked → working → settled. Settled is
`idle` when completion has been seen and `done` when unseen, not a failure.
No approval was requested or granted, and caller focus was preserved. Close
only the temporary pane created for the fixture; never the user's session.
