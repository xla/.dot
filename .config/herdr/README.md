# Shady and Exposed for Herdr

`config.toml` brings the [Pi Shady palette](../../.pi/README.md) and its readable
light counterpart [Exposed](../exposed/README.md) into Herdr's sidebar, tab bar,
pane focus, menus, and agent states. It now follows the system-aware terminal
appearance automatically. Tested with Herdr **0.9.3** and Pi **1.0.4**.

This guide documents the dark Shady branch. The Exposed guide documents the
shared palette generator, contrast guarantees, and light-mode live test.

Herdr 0.9.3 has no named custom-theme files or include mechanism. The theme is
implemented as all **19** supported tokens in `[theme.custom.light]` (Exposed)
and `[theme.custom.dark]` (Shady), with **`auto_switch = true`**. Named palette
sources live under `themes/`. Valid built-in bases `one-light` / `vesper` are
fully overridden. Do not set `name = "exposed"` or `name = "shady"`: Herdr
diagnoses unknown names and falls back. Mode-specific overrides are supported;
unknown custom base names and includes are not.

## Install

From this dotfiles checkout, inspect and back up an existing config first:

```sh
mkdir -p ~/.config/herdr
# If present, preserve your settings before replacing this specific file.
cp -p ~/.config/herdr/config.toml ~/.config/herdr/config.toml.pre-shady
# Only after checking the backup/destination:
rm ~/.config/herdr/config.toml
ln -s "$PWD/.config/herdr/config.toml" ~/.config/herdr/config.toml
herdr config check
herdr server reload-config
```

Do not replace the whole `~/.config/herdr` directory: it also contains sockets,
logs, and persistent sessions. Merge any additional personal settings into the
repository config before switching. This machine's existing onboarding and
agent-sort settings were preserved, with the original file backed up as
`config.toml.pre-shady`.

For accurate Pi lifecycle reporting rather than screen-scraping heuristics:

```sh
herdr integration install pi
```

This installs Herdr's official managed extension at
`~/.pi/agent/extensions/herdr-agent-state.ts`. It was previously absent on this
machine and is now installed. New Pi sessions load it; `/reload` loads it in an
already-open session. The file is managed by Herdr, not copied into dotfiles.

## Color and state rules

Neutral chrome stays neutral: black sidebar, `#121212` panels/active rows,
`#262626` Navigate selections, `#c6c6c6` body text, and readable `#949494`
secondary labels/focus accents. Branch labels use Shady identifier blue
`#87afaf`, not inherited purple.

| Element/state | Herdr token | Color | Additional cue |
| --- | --- | --- | --- |
| Idle/ready | `green` | `#7fffd4` | `○`, state text |
| Working | `yellow` | `#ffe4b5` | `◐`, state text |
| Blocked/needs attention | `red` | `#d76b91` | `×`, state text |
| Unseen completion | `teal` | `#add8e6` | `✓`, state text |
| Notification/unseen accents | `blue` | `#add8e6` | Native notification markers |
| Warning/interruption | `peach` | `#ffe4b5` | Native label/context |
| Unknown | `overlay0` | `#949494` | `·` |
| Pane focus/navigation | `accent` | `#949494` | Bright border/selection position |

Agent rows show state icon/text together, followed by agent and location.
This avoids relying on colored dots alone. No inline token color overrides
mask the native contextual state colors. Herdr 0.9.3's expanded-agent state
text treats unknown as idle; its unknown glyph/color still differ. Actual
`done` versus `idle` depends on whether completion has been seen: an unfocused
completed pane can be `done`, while focus marks it seen.

### Contrast and intentional changes to Pi

The original Shady maroon `#b03060` is only **2.48:1** on selection gray and
**3.07:1** on Pi tool panels. Both Herdr attention and Pi errors/removed diff
lines now use a brighter same-family maroon `#d76b91`:

- Functional Herdr text/state tokens meet **4.5:1** against all six UI surfaces
  (72 tested combinations), minimum **4.61:1**.
- Body text on selection gray is **8.86:1**.
- Pi error/removed-line text on its standard tool panel is **5.71:1**.
- Failed Pi tools alone use a subdued `#27131b` panel, distinguishing them
  from pending/success output without tinting every tool result.

These are sRGB contrast checks for the configured truecolor palette, not a
claim that every terminal/theme is universally accessible. Decorative
`surface_dim` separators remain subdued; they do not exclusively communicate
state. Pi's original dim syntax comments and thinking-text roles remain as in
Neovim. In 256-color-only terminals RGB state hues are approximated. The pane
canvas is still owned by the terminal/Pi rather than recolored by Herdr.

## Validation

```sh
python3 .config/herdr/tests/validate-shady.py
PI_PACKAGE="$(brew --prefix pi-coding-agent)/libexec/lib/node_modules/@earendil-works/pi-coding-agent"
PI_OFFLINE=1 node .pi/scripts/validate-shady.mjs "$PI_PACKAGE" --installed --nvim
```

The first checks the complete Herdr override set, shared Pi palette, state
cues, contrast pairs, and Herdr's actual config parser. The second checks Pi's
schema, real renderer/loading/export, and live Neovim reference highlights.
The attention-color lift is an explicit, documented exception to exact Vim
color fidelity.

### Repeatable real Pi-in-Herdr session

`tests/pi-session.ts` is an **explicitly loaded test-only extension**, outside
Pi's auto-discovery directories. It provides a deterministic offline model:
no external model service or credentials are used. Pi itself runs its real
interactive UI and real built-in read/edit/bash tools.

For this **Shady** fixture, use a dark host terminal. For the light, automatically
selected **Exposed** variant, follow the Exposed guide linked above.
From inside a Herdr pane (`HERDR_ENV=1`), with the official Pi integration
installed, prepare a disposable run and sibling pane:

```sh
RUN=$(mktemp -d "${TMPDIR:-/tmp}/herdr-shady-session-XXXXXX")
mkdir -p "$RUN/agent/themes" "$RUN/sessions"
ln -s "$PWD/.pi/agent/themes/shady.json" "$RUN/agent/themes/shady.json"
printf '{"theme":"shady","quietStartup":true,"packages":[]}\n' > "$RUN/agent/settings.json"
# Use down instead of right for a narrow current pane.
SPLIT=$(herdr pane split --current --direction right --cwd "$PWD" --no-focus)
# Parse the returned ID; do not guess or reuse an unrelated pane.
PANE=$(printf '%s' "$SPLIT" | python3 -c 'import json,sys; print(json.load(sys.stdin)["result"]["pane"]["pane_id"])')
herdr pane run "$PANE" "env PI_CODING_AGENT_DIR='$RUN/agent' SHADY_SESSION_DIR='$RUN' PI_OFFLINE=1 pi --no-approve --no-extensions --no-skills --no-prompt-templates --no-context-files --extension '$HOME/.pi/agent/extensions/herdr-agent-state.ts' --extension '$PWD/.config/herdr/tests/pi-session.ts' --provider shady-fixture --model session --thinking off --use-theme shady --session-dir '$RUN/sessions' 'Run the offline Shady coding session fixture.'"
```

The fixture writes `completed.json` once settled. Validate the retained run:

```sh
python3 .config/herdr/tests/validate-shady.py --run-dir "$RUN"
```

The run includes a user prompt, TypeScript read, real edit and colored diff,
intentional failed bash check, Markdown heading/list/code/link, then a completed
assistant response. Herdr's official integration reports idle → working →
**simulated** blocked → working → settled (`done` if unseen, otherwise `idle`).
The blocked event tests the
attention channel; it is not a real approval and none is granted. The validator
checks the session transcript and actual ANSI colors read back through Herdr's
virtual terminal, including brighter removals and the distinct error panel.
Every observation verifies that caller focus was not stolen.

Only the fixture's guarded temporary `sample.ts` is modified. Personal Pi
settings/auth are isolated; project resources are ignored with `--no-approve`.
The visible untrusted-project warning is expected and uses Shady warning color.
The test does not save trust, install packages, or touch project source files.

When finished, close **only the pane created for this test**:

```sh
herdr pane close "$PANE"
```

## Source references

The supported keys and role behavior were checked against the installed
binary and version-pinned Herdr sources:

- [Config/color schema](https://github.com/herdrdev/herdr/blob/v0.9.3/src/config/theme.rs)
- [Palette application](https://github.com/herdrdev/herdr/blob/v0.9.3/src/app/state.rs)
- [Status glyphs and colors](https://github.com/herdrdev/herdr/blob/v0.9.3/src/client/shell.rs)
- [Official Pi lifecycle integration](https://github.com/herdrdev/herdr/blob/v0.9.3/src/integration/assets/pi/herdr-agent-state.ts)

The validators intentionally target these installed versions. New Herdr color
roles or Pi internal API changes require a reviewed test update.
