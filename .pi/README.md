# Shady for Pi

`.pi/agent/themes/shady.json` ports the **active terminal highlights** from
`.vim/pack/minpac/start/shady.vim/colors/shady.vim`, as loaded by `.vimrc`.
Neovim uses `notermguicolors`, so the `cterm` values—not the inherited GUI
colors—are the reference. Named ANSI colors are resolved through the existing
Kitty/Ghostty Shady palette in `.config/ghostty/themes/shady`.

[Exposed](../.config/exposed/README.md) is the readable light counterpart.
The installed setup now automatically chooses **Exposed in light mode and
Shady in dark mode**; this guide documents the Shady palette.

## Install and select

From this dotfiles checkout:

```sh
mkdir -p ~/.pi/agent/themes
ln -s "$PWD/.pi/agent/themes/shady.json" ~/.pi/agent/themes/shady.json
ln -s "$PWD/.pi/agent/themes/exposed.json" ~/.pi/agent/themes/exposed.json
```

If that destination already exists, inspect it before replacing it. Select
**exposed/shady** automatic mode in Pi's `/settings`, or set just
`"theme": "exposed/shady"` in `~/.pi/agent/settings.json`, preserving other
settings. The order is light/dark. Select `shady` alone to opt out of switching.

Run `/reload` in an already-open Pi session after installation or a manual
settings change. New sessions use the saved theme automatically. To try it
without changing settings, run `pi --use-theme shady`.

The source lives under `.pi/agent/themes/`, **not** the auto-discovered project
`.pi/themes/` directory. Only the user-level symlink is discovered, so the theme
works in every project and does not create a duplicate Shady resource in this
checkout. Pi watches that user-level filename for theme updates; `/reload`
explicitly refreshes it if a symlink target edit is not noticed.

## Palette and rules

- Keep the terminal's background (black in the existing terminal setup).
- Grayscale body text, interface chrome, panels, and thinking-level borders.
- Reserve blue for identifiers/types and pale green for numbers; no rainbow
  syntax highlighting or colored success panel fills. Failed tools alone use
  a subdued `#27131b` background so errors do not look like successful output.
- Use Shady-family maroon, aquamarine, moccasin, and light blue for errors,
  additions/success, warnings, and links. Errors and removed lines lift the
  original maroon to `#d76b91` for readable small text, shared with Herdr.
- Keep dim comments and structural details dim, as in Shady; this intentionally
  does not brighten every role to a minimum-contrast accessibility palette.

| Pi roles | Shady reference | Value |
| --- | --- | --- |
| Body, messages, tool output, diff context | `Normal` | 251 / `#c6c6c6` |
| Keywords, tool titles, selection accent | `Keyword` / `Bright` | 253 / `#dadada` |
| Functions and operators | live `Function`, `Operator` | 248 / `#a8a8a8` |
| Variables and types | `Identifier`, `Type` | 109 / `#87afaf` |
| Strings and punctuation | `String`, `Delimiter` | 245 / `#8a8a8a` |
| Numbers | `Number` | 193 / `#d7ffaf` |
| Comments, thinking text, dim UI hints | `Comment` | 239 / `#4e4e4e` |
| Markdown heading | `markdownH1` | 254 / `#e4e4e4` |
| Markdown code | `markdownCode`, `markdownCodeBlock` | 246 / `#949494` |
| Markdown quote | `markdownBlockquote` | 242 / `#6c6c6c` |
| Markdown fences/rules | `markdownCodeDelimiter`, `markdownRule` | 238 / `#444444` |
| Markdown bullets, secondary UI text | `GreyBold`, `TabLineSel` | 245 / `#8a8a8a` |
| Borders / muted borders | `StatusLine`, `VertSplit` | 238 / 236 |
| Selection | `Visual` | background 235 / `#262626` |
| User/tool panels | `Pmenu` | background 233 / `#121212` |
| Error, removed diff lines | `Error`, `diffRemoved` → ANSI 9, brightened for contrast | `#d76b91` |
| Failed tool panel | Pi-only restrained maroon surface | background `#27131b` |
| Success, added diff lines | `DiffAdd`, `diffAdded` → ANSI 10 | `#7fffd4` |
| Warning | `Warning` → ANSI 11 | `#ffe4b5` |
| Markdown links and URLs | `markdownLink` → ANSI 14 | `#add8e6` |

The extended palette uses numeric indices to retain Neovim's exact terminal
colors. Named ANSI colors use explicit RGB from your terminal palette: otherwise
Pi's HTML export interprets index 9 as generic bright red rather than your
maroon. The original ANSI 9 maroon is retained as the source variable `red`,
but the `attention` variable brightens errors and removed lines: original
`#b03060` is only 3.07:1 on tool panels, while `#d76b91` is 5.71:1. The brighter
color is shared with the [Herdr Shady theme](../.config/herdr/README.md).
These RGB colors are exact in truecolor terminals and approximate in
256-color-only terminals. No terminal capability override is installed.

HTML export explicitly uses black pages, `#121212` cards, and `#262626` info
panels; the terminal canvas itself remains controlled by the terminal.

### Pi format limitations

This is a color-role port, not an editor/highlighter replacement:

- Pi theme JSON cannot specify per-token bold/italic/undercurl attributes.
  Markdown still uses Pi's native bold and italic rendering, but Vim's bold
  keywords and italic syntax comments/code cannot be prescribed by the theme.
- Pi has one heading role, so H1–H6 share Shady's H1 color rather than Vim's
  descending heading shades.
- Pi uses its own syntax classifier rather than Neovim's Tree-sitter captures
  or language-specific highlight links. For example, Pi's literal tokens
  (including booleans) share its number role. Variables use Shady's generic
  `Identifier` role; some language-specific Neovim variables are plain text.
- Pi search requires two colors rather than Vim's reverse attribute. It uses
  black text on the body-text gray, with Pi reversing those colors for the
  current match. Menu selection uses Shady's `Visual` gray.
- Pi-only thinking levels use a grayscale brightness progression, and Bash
  mode uses the existing identifier blue. Status labels convey tool state
  without tinted success/pending backgrounds. Failed panels alone are tinted
  to distinguish failures from ordinary output.

## Validation

The regression script uses the **installed Pi validator and renderer**, with
no model calls. Pass Pi's package directory (the folder containing its
`package.json`, `dist/`, and `node_modules/`). For this Homebrew installation:

```sh
PI_PACKAGE="$(brew --prefix pi-coding-agent)/libexec/lib/node_modules/@earendil-works/pi-coding-agent"
PI_OFFLINE=1 node .pi/scripts/validate-shady.mjs "$PI_PACKAGE" --installed --nvim
```

- Default: validate all 56 current color roles, source palette expectations,
  and actual Markdown/syntax rendering at 32/80/120 columns in truecolor and
  256-color modes. Checks reject an incomplete theme as a negative control.
- `--nvim`: also compare 20 role mappings and 5 ANSI roles against the active
  local Neovim highlights using a temporary headless session.
- `--installed`: verify the installed symlink, effective settings, Pi's actual
  theme discovery, absence of theme collisions/errors, unique selector entry,
  activation of the automatic pair's dark branch without silent fallback,
  and resolved HTML export colors.
- `--preview`: print the actual 80-column Markdown render for terminal review.

`--nvim` requires this machine's Shady-based Neovim setup; `--installed` requires
the installation above. Other machines can run the default checks first.
The script imports Pi's internal validation/rendering modules, so a Pi upgrade
may require updating import paths or new color-role expectations.

## Completion audit

Validated against Pi **1.0.4** and the local live Neovim configuration:

- All 56 schema roles present and accepted by Pi's real validator.
- All 20 mapped terminal highlights and 5 ANSI groups match live Neovim.
- Success/warning/link RGB values match the existing Shady terminal palette;
  error and removed-line maroon is intentionally brightened for contrast.
- Both color modes render Markdown and highlighted code at all three widths.
- User installation resolves to this repository file; saved effective theme is
  `exposed/shady`; the dark branch resolves to `shady`; real Pi discovery
  reports one Shady theme and no theme diagnostics.
- Pi activation returns success and retains the Shady name (no fallback).
- HTML colors resolve correctly, including the contrast-adjusted maroon.
- A real Pi TUI inside Herdr additionally exercises tool calls, diffs, failed
  tools, code, links, and official lifecycle reports; see the Herdr guide.

An already-running session still needs `/reload` to apply newly saved settings;
that UI action is not performed by the validation script.
