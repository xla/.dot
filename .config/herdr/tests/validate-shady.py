#!/usr/bin/env python3
"""Static palette/contrast audit, optionally including a real Pi-in-Herdr fixture run.
Usage: python3 .config/herdr/tests/validate-shady.py [--run-dir PATH]
Requires Python 3.11+ and Herdr. Does not change config or control any panes.
"""
import argparse
import json
import os
from pathlib import Path
import re
import subprocess
import tomllib

REPO = Path(__file__).resolve().parents[3]
CONFIG = REPO / '.config/herdr/config.toml'
args = argparse.ArgumentParser(description=__doc__)
args.add_argument('--run-dir', type=Path)
args = args.parse_args()
config = tomllib.loads(CONFIG.read_text())
colors = config['theme']['custom']['dark']
pi = json.loads((REPO / '.pi/agent/themes/shady.json').read_text())
required = set('accent panel_bg sidebar_bg active_row_bg selection_bg surface0 surface1 surface_dim overlay0 overlay1 text subtext0 mauve green yellow red blue teal peach'.split())
assert set(colors) == required, 'Override every Herdr 0.9.3 color; no inherited rainbow roles'
assert config['theme']['name'] == 'vesper', 'Use a supported base, not an unknown shady name'
assert config['theme']['auto_switch'] is True
assert config['theme']['dark_name'] == 'vesper'
assert config['theme']['light_name'] == 'one-light'
assert config['ui']['status_indicators'] == 'symbols'
assert config['ui']['sidebar']['agents']['rows'][0] == ['state_icon', 'state_text']
for key, color in colors.items():
    assert re.fullmatch(r'#[0-9a-f]{6}', color), f'Explicit RGB: {key}'

def pi_color(token):
    value = pi['colors'][token]
    while isinstance(value, str) and value in pi['vars']:
        value = pi['vars'][value]
    if isinstance(value, int):
        if value >= 232:
            rgb = [8 + (value - 232) * 10] * 3
        else:
            assert value >= 16, 'No ambiguous terminal ANSI overrides'
            n = value - 16
            cube = [0, 95, 135, 175, 215, 255]
            rgb = [cube[n // 36], cube[n // 6 % 6], cube[n % 6]]
        return '#' + ''.join(f'{n:02x}' for n in rgb)
    return value or '#000000'

for role, token in {'panel_bg': 'toolPendingBg', 'selection_bg': 'selectedBg',
                    'text': 'text', 'mauve': 'syntaxType', 'green': 'success',
                    'yellow': 'warning', 'red': 'error', 'blue': 'mdLink', 'teal': 'mdLink'}.items():
    assert colors[role] == pi_color(token), f'Herdr {role} and Pi {token} stay coherent'
assert pi_color('toolDiffRemoved') == colors['red']
assert pi_color('toolErrorBg') != colors['panel_bg']
for role in 'panel_bg sidebar_bg active_row_bg selection_bg surface0 surface1 surface_dim overlay0 overlay1 text subtext0 accent'.split():
    c = colors[role]
    assert c[1:3] == c[3:5] == c[5:7], f'Neutral chrome: {role}'
assert colors['selection_bg'] != colors['active_row_bg'], 'Navigate cursor is not the active row'

def luminance(color):
    rgb = [int(color[i:i + 2], 16) / 255 for i in (1, 3, 5)]
    linear = [c / 12.92 if c <= 0.04045 else ((c + 0.055) / 1.055) ** 2.4 for c in rgb]
    return sum(a * b for a, b in zip(linear, (0.2126, 0.7152, 0.0722)))

def contrast(fg, bg):
    a, b = sorted((luminance(fg), luminance(bg)))
    return (b + 0.05) / (a + 0.05)

ratios = []
for fg in 'text subtext0 overlay0 overlay1 accent mauve red green yellow blue teal peach'.split():
    for bg in 'panel_bg sidebar_bg active_row_bg selection_bg surface0 surface1'.split():
        ratio = contrast(colors[fg], colors[bg])
        assert ratio >= 4.5, f'{fg}/{bg}: {ratio:.2f}:1 < 4.5:1'
        ratios.append((ratio, fg, bg))
for token in ['error', 'toolDiffRemoved', 'toolDiffAdded', 'toolTitle', 'toolOutput']:
    for bg in ['toolPendingBg', 'toolErrorBg']:
        assert contrast(pi_color(token), pi_color(bg)) >= 4.5, f'Pi {token}/{bg}'
minimum, fg, bg = min(ratios)
print(f'PASS 19 complete Herdr roles, shared Pi palette, state symbols/text, and {len(ratios)} contrast pairs')
print(f'Minimum functional contrast: {minimum:.2f}:1 ({fg} on {bg}); body/selection: {contrast(colors["text"], colors["selection_bg"]):.2f}:1')
# Decorative separators intentionally remain subdued; they carry no exclusive state.
env = {**os.environ, 'HERDR_CONFIG_PATH': str(CONFIG)}
result = subprocess.run(['herdr', 'config', 'check'], env=env, capture_output=True, text=True, timeout=10, check=True)
assert result.stdout.strip() == 'config: ok', result.stdout + result.stderr
assert not result.stderr.strip(), result.stderr
print('PASS installed Herdr parser accepts the configuration without diagnostics')

if args.run_dir:
    run = args.run_dir.resolve()
    completed = json.loads((run / 'completed.json').read_text())
    assert completed['turns'] == 4
    expected = {'idle': 'idle', 'working': 'working', 'blocked': 'blocked', 'diff': 'working', 'error': 'working', 'settled': 'done'}
    assert len(completed['observations']) == len(expected)
    observed = {o['label']: o for o in completed['observations']}
    for label, status in expected.items():
        o = observed[label]
        assert o['theme'] == 'shady'
        assert o['pane']['agent'] == 'pi'
        states = ['idle', 'done'] if label == 'settled' else [status]
        assert o['pane']['agent_status'] in states, (label, o['pane']['agent_status'])
        assert o['pane']['focused'] is False, 'Do not steal caller focus'
        assert o['pane']['agent_session']['source'] == 'herdr:pi'
    def frame(label):
        return (run / f'{label}.ansi').read_text()
    final = frame('settled')
    for text in ['Shady session complete', 'Ready for input.', 'fixture check failed', 'Command exited with code 1', 'greeting']:
        assert text in final, text
    for code in ['38;5;253m', '38;5;109m', '38;5;193m', '38;5;239m',
                 '38;2;215;107;145m', '38;2;127;255;212m', '38;2;173;216;230m',
                 '48;2;39;19;27m']:
        assert '\x1b[' + code in final, f'Actual Herdr VT preserves {code}'
    assert 'ATTENTION: simulated test state' in frame('blocked')
    assert '\x1b[38;2;215;107;145m' in frame('blocked')
    records = [json.loads(line) for path in (run / 'sessions').glob('*.jsonl') for line in path.read_text().splitlines()]
    results = [j['message'] for j in records if j.get('message', {}).get('role') == 'toolResult']
    assert [(m['toolName'], m['isError']) for m in results] == [('read', False), ('edit', False), ('bash', True)]
    assert '"shady"' in (run / 'sample.ts').read_text()
    print('PASS real Pi TUI: read, successful edit/diff, deliberate failed tool, Markdown/code/link, actual VT colors, official lifecycle reports, and unchanged focus')
    print('Observed idle → working → simulated blocked → working → settled (idle/done depending seen state); no approval interaction')
