#!/usr/bin/env python3
"""Audit generated Exposed themes, actual config parsers, and live Neovim switching.
Usage: python3 .config/exposed/validate.py [--run-dir PATH]
Python 3.11+, Neovim, Ghostty, Herdr required. Does not change system appearance.
"""
import argparse
import json
import os
from pathlib import Path
import re
import subprocess
import tempfile
import tomllib

ROOT = Path(__file__).resolve().parents[2]
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--run-dir', type=Path)
args = parser.parse_args()

def run(argv, **kwargs):
    return subprocess.run(argv, capture_output=True, text=True, check=True, timeout=25, **kwargs)

run(['python3', str(ROOT / '.config/exposed/generate.py'), '--check'])
source = json.loads((ROOT / '.config/exposed/palette.json').read_text())
assert source['name'] == 'exposed' and source['appearance'] == 'light'

def hex_color(i):
    if i >= 232:
        values = [8 + 10 * (i - 232)] * 3
    else:
        assert i >= 16
        n, cube = i - 16, [0, 95, 135, 175, 215, 255]
        values = [cube[n // 36], cube[n // 6 % 6], cube[n % 6]]
    return '#' + ''.join(f'{v:02x}' for v in values)

def luminance(c):
    v = [int(c[i:i + 2], 16) / 255 for i in (1, 3, 5)]
    return sum(w * (c / 12.92 if c <= .04045 else ((c + .055) / 1.055) ** 2.4) for c, w in zip(v, (.2126, .7152, .0722)))

def contrast(a, b):
    lo, hi = sorted((luminance(a), luminance(b)))
    return (hi + .05) / (lo + .05)

assert contrast('#eeeeee', '#ffffff') < 4.5, 'Negative control: pale text on white fails'
herdr = tomllib.loads((ROOT / '.config/herdr/config.toml').read_text())
assert herdr['theme']['auto_switch'] is True
assert (herdr['theme']['dark_name'], herdr['theme']['light_name']) == ('vesper', 'one-light')
assert herdr['ui']['status_indicators'] == 'symbols'
assert herdr['ui']['sidebar']['agents']['rows'][0] == ['state_icon', 'state_text']
light = tomllib.loads((ROOT / '.config/herdr/themes/exposed.toml').read_text())
dark = tomllib.loads((ROOT / '.config/herdr/themes/shady.toml').read_text())
assert herdr['theme']['custom']['light'] == light and herdr['theme']['custom']['dark'] == dark
assert set(light) == set(dark) and len(light) == 19
surfaces = ['sidebar_bg', 'panel_bg', 'active_row_bg', 'selection_bg', 'surface0', 'surface1']
foregrounds = set(light) - set(surfaces)
pairs = [(contrast(light[fg], light[bg]), fg, bg) for fg in foregrounds for bg in surfaces]
assert min(r for r, _, _ in pairs) >= 4.5, sorted(pairs)[:3]
assert min(contrast(light['surface_dim'], light[bg]) for bg in surfaces) >= 3
# Herdr's config-diagnostic banner uses panel_bg as foreground on yellow;
# selection/status badges also reverse text/panel pairs. Check these usages.
assert contrast(light['panel_bg'], light['yellow']) >= 4.5
assert contrast(light['panel_bg'], light['text']) >= 4.5
result = run(['herdr', 'config', 'check'], env={**os.environ, 'HERDR_CONFIG_PATH': str(ROOT / '.config/herdr/config.toml')})
assert result.stdout.strip() == 'config: ok' and not result.stderr.strip()
print(f'PASS Herdr: 19 Exposed overrides, native auto-switch configuration, {len(pairs)} text/border pairs; minimum {min(pairs)[0]:.2f}:1')

ghostty = '/Applications/Ghostty.app/Contents/MacOS/ghostty'
if not Path(ghostty).exists():
    ghostty = 'ghostty'
result = run([ghostty, '+validate-config', '--config-file=' + str(ROOT / '.config/ghostty/config.ghostty')])
assert not result.stderr.strip(), result.stderr
settings = (ROOT / '.config/ghostty/config.ghostty').read_text()
assert re.search(r'^theme = light:exposed,dark:shady$', settings, re.M)
assert re.search(r'^minimum-contrast = 1$', settings, re.M)
assert re.search(r'^background-opacity = 1$', settings, re.M)
compiled = {}
for name, expected_bg in [('exposed', '#ffffff'), ('shady', '#000000')]:
    # show-config only accepts its own flags; isolate config discovery rather
    # than passing unsupported general config flags or changing user settings.
    with tempfile.TemporaryDirectory(prefix='exposed-ghostty-') as temp:
        directory = Path(temp) / 'ghostty'
        directory.mkdir()
        (directory / 'config.ghostty').write_text(f'theme = {ROOT}/.config/ghostty/themes/{name}\nminimum-contrast = 1\n')
        text = run([ghostty, '+show-config', '--changes-only'], env={**os.environ, 'XDG_CONFIG_HOME': temp}).stdout
    assert f'background = {expected_bg}' in text
    compiled[name] = text
ansi = dict((int(i), h) for i, h in re.findall(r'^palette = (\d+)=(#[0-9a-f]{6})$', compiled['exposed'], re.M))
assert len(ansi) == 256
for i in range(16, 256):
    assert ansi[i] == hex_color(i), 'Keep extended palette exact'
for i in range(16):
    for bg in surfaces:
        assert contrast(ansi[i], light[bg]) >= 4.5, (i, bg)
for fg, bg in [('foreground', 'background'), ('selection-foreground', 'selection-background'), ('cursor-color', 'cursor-text')]:
    a = re.search(r'^' + fg + r' = (#[0-9a-f]{6})$', compiled['exposed'], re.M)
    b = re.search(r'^' + bg + r' = (#[0-9a-f]{6})$', compiled['exposed'], re.M)
    assert a and b and contrast(a[1], b[1]) >= 4.5
print('PASS Ghostty: actual config loader, native system pair, all 16 ANSI foregrounds, selection/cursor, exact extended palette, and unmodified palette rendering')

with tempfile.TemporaryDirectory(prefix='exposed-nvim-') as temp:
    path = str(Path(temp) / 'highlights.json')
    # get_hl(link=false) without a name returns an unresolved map in this Nvim build.
    # Query names individually to audit actual resolved links, never stale aliases.
    lua = ('local result={}; for _,mode in ipairs({"light","dark","light"}) do '
           'vim.o.background=mode; local h={}; if mode=="light" then '
           'for name,_ in pairs(vim.api.nvim_get_hl(0,{})) do h[name]=vim.api.nvim_get_hl(0,{name=name,link=false}) end end; '
           'table.insert(result,{theme=vim.g.colors_name,background=vim.o.background,ansi=vim.g.terminal_ansi_colors,highlights=h}) end; '
           f'vim.fn.writefile({{vim.json.encode(result)}},{json.dumps(path)})')
    result = run(['nvim', '--headless', '--cmd', 'set background=light', '-c', 'lua ' + lua, '-c', 'qa!'], cwd=ROOT)
    phases = json.loads(Path(path).read_text())
    assert [(p['theme'], p['background']) for p in phases] == [('exposed', 'light'), ('shady', 'dark'), ('exposed', 'light')]
    expected_ansi = [ansi[n] for n in range(16)]
    assert phases[0]['ansi'] == expected_ansi and phases[2]['ansi'] == expected_ansi
    assert phases[1]['ansi'][0] == '#000000' and phases[1]['ansi'][15] == '#ffffff', 'Embedded terminal returns to Shady'
    count = 0
    for phase in [phases[0], phases[2]]:
        highlights = phase['highlights']
        normal = highlights['Normal']
        for name, value in highlights.items():
            # This plugin utility deliberately hides the hardware cursor. It
            # renders no text; making it opaque would break CoC popup input.
            if name == 'CocCursorTransparent':
                assert value.get('blend') == 100
                continue
            assert not value.get('blend'), f'{name}: no translucent text'
            for fg, bg in [('fg', 'bg'), ('ctermfg', 'ctermbg')]:
                a, b = value.get(fg, normal[fg]), value.get(bg, normal[bg])
                if fg == 'fg':
                    a, b = f'#{a:06x}', f'#{b:06x}'
                else:
                    a, b = ansi[a], ansi[b]
                ratio = contrast(a, b)
                assert ratio >= 4.5, f'{name} {fg}/{bg}: {ratio:.2f}:1 ({a}/{b})'
                count += 1
            if (value.get('underline') or value.get('undercurl')) and 'sp' in value:
                assert contrast(f'#{value["sp"]:06x}', f'#{value.get("bg", normal["bg"]):06x}') >= 3, f'{name}: underline/undercurl contrast'
        for group in ['Normal', 'Comment', 'Number', 'Type', 'Visual', 'Pmenu', 'CocFloating', 'CocErrorSign', '@keyword', '@markup.link']:
            assert group in highlights
    print(f'PASS live Neovim: native background light/dark/light reload, {len(phases[0]["highlights"])} resolved groups, {count} GUI/cterm contrast checks including installed plugins')

if args.run_dir:
    root = args.run_dir.resolve()
    completed = json.loads((root / 'completed.json').read_text())
    assert completed['turns'] == 4
    expected = {'idle': 'idle', 'working': 'working', 'blocked': 'blocked', 'diff': 'working', 'error': 'working', 'settled': 'done'}
    assert len(completed['observations']) == len(expected)
    for observation in completed['observations']:
        assert observation['theme'] == 'exposed', 'Automatic appearance detection selects Exposed'
        pane = observation['pane']
        label = observation['label']
        states = ['idle', 'done'] if label == 'settled' else [expected[label]]
        assert pane['agent'] == 'pi' and pane['agent_status'] in states, (label, pane['agent_status'])
        assert pane['focused'] is False and pane['agent_session']['source'] == 'herdr:pi'
    final = (root / 'settled.ansi').read_text()
    for token in ['Exposed session complete', 'Ready for input.', 'fixture check failed', 'Command exited with code 1']:
        assert token in final
    for code in ['38;5;234m', '38;5;23m', '38;5;22m', '38;5;240m', '38;5;89m', '38;5;24m', '48;5;224m']:
        assert '\x1b[' + code in final, f'Actual Herdr VT preserves {code}'
    assert '\x1b[38;5;89m' in (root / 'blocked.ansi').read_text()
    transcript = [json.loads(l) for f in (root / 'sessions').glob('*.jsonl') for l in f.read_text().splitlines()]
    results = [j['message'] for j in transcript if j.get('message', {}).get('role') == 'toolResult']
    assert [(m['toolName'], m['isError']) for m in results] == [('read', False), ('edit', False), ('bash', True)]
    assert '"exposed"' in (root / 'sample.ts').read_text()
    print('PASS real automatically selected Exposed Pi-in-Herdr session: read/edit/diff/error/Markdown/code/link, actual VT colors, simulated attention, lifecycle states, and preserved focus')
