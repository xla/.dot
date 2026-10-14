#!/usr/bin/env python3
"""Exercise actual Neovim and Pi TUIs under an isolated terminal protocol emulator.
Sends light → dark → light Mode 2031 notifications and answers OSC color queries.
No system preference changes, model calls, real terminal input, or Herdr controls.
Run as a background task; typically completes in a few seconds.
"""
import errno
import fcntl
import json
import os
from pathlib import Path
import pty
import re
import select
import signal
import struct
import subprocess
import tempfile
import termios
import time

ROOT = Path(__file__).resolve().parents[2]
PROBE = '''import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { writeFileSync } from "node:fs";
export default function(pi: ExtensionAPI) {
  let timer: ReturnType<typeof setInterval> | undefined;
  pi.registerProvider("exposed-probe", {
    baseUrl: "http://127.0.0.1/unused", apiKey: "offline-no-network", api: "exposed-probe",
    models: [{ id: "idle", name: "Appearance probe", reasoning: false, input: ["text"],
      cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 }, contextWindow: 32000, maxTokens: 1024 }],
    streamSimple() { throw new Error("Appearance probe must never call a model"); }
  });
  pi.on("session_start", (_event, ctx) => {
    timer = setInterval(() => writeFileSync(process.env.EXPOSED_PROBE_FILE!, JSON.stringify({theme:ctx.ui.theme.name})), 100);
  });
  pi.on("session_shutdown", () => { if (timer) clearInterval(timer); });
}
'''

# Full sequence buffering, including queries split across writes.
QUERY = re.compile(rb'\x1b\](10|11);\?(\x07|\x1b\\)|\x1b\]4;(\d+);\?(\x07|\x1b\\)|\x1b\[\?2031\$p|\x1b\[\?996n|\x1b\[(?:0)?c')

def exercise(name, argv, env, status):
    master, slave = pty.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', 40, 100, 0, 0))
    child = subprocess.Popen(argv, stdin=slave, stdout=slave, stderr=slave, env=env,
                             cwd=ROOT, start_new_session=True)
    os.close(slave)
    buffer, transcript = b'', bytearray()
    mode, index, color_queries, reports_enabled = 'light', 0, 0, False
    expected = ['exposed', 'shady', 'exposed']
    deadline = time.monotonic() + 30
    next_notice = time.monotonic() + .75
    observed = []
    try:
        while index < 3 and time.monotonic() < deadline:
            if child.poll() is not None:
                raise AssertionError(f'{name} exited {child.returncode}: {transcript[-2000:]!r}')
            if select.select([master], [], [], .05)[0]:
                try:
                    data = os.read(master, 65536)
                except OSError as e:
                    if e.errno == errno.EIO:
                        break
                    raise
                buffer += data
                transcript.extend(data)
                if b'\x1b[?2031h' in transcript:
                    reports_enabled = True
                while match := QUERY.search(buffer):
                    code = match[1]
                    if code in [b'10', b'11']:
                        value = b'3030/3030/3030' if code == b'10' else b'ffff/ffff/ffff'
                        if mode == 'dark':
                            value = b'ffff/ffff/ffff' if code == b'10' else b'0000/0000/0000'
                        answer = b'\x1b]' + code + b';rgb:' + value + b'\x1b\\'
                        color_queries += 1
                    elif match[3] is not None:
                        i = int(match[3]); light = mode == 'light'
                        colors = ['000000','87005f','005f00','5f5f00','5f0087','87005f','005f87','303030',
                                  '585858','87005f','005f00','5f5f00','5f0087','87005f','005f87','1c1c1c'] if light else [
                                  '000000','b03060','66cdaa','ffe4b5','483d8b','663399','add8e6','bebebe',
                                  '404040','b03060','7fffd4','ffe4b5','483d8b','663399','add8e6','ffffff']
                        color = colors[i % 16]
                        value = '/'.join(color[p:p+2] * 2 for p in (0, 2, 4)).encode()
                        answer = b'\x1b]4;' + match[3] + b';rgb:' + value + b'\x1b\\'
                    elif match[0] == b'\x1b[?2031$p':
                        answer = b'\x1b[?2031;2$y'
                    elif match[0] in [b'\x1b[c', b'\x1b[0c']:
                        # DA1 ends a color-query batch. Without it, late replies
                        # stay assigned to an old query rather than the next one.
                        answer = b'\x1b[?62;22c'
                    else:
                        answer = b'\x1b[?997;' + (b'2' if mode == 'light' else b'1') + b'n'
                    os.write(master, answer)
                    buffer = buffer[match.end():]
                # Unrelated TUI redraws can be large; retain enough for split sequences.
                buffer = buffer[-4096:]
            if time.monotonic() >= next_notice:
                # Only emit after the app has requested appearance notifications.
                if reports_enabled:
                    os.write(master, b'\x1b[?997;' + (b'2' if mode == 'light' else b'1') + b'n')
                next_notice = time.monotonic() + .5
            if status.exists():
                try:
                    state = json.loads(status.read_text())
                except json.JSONDecodeError:
                    continue
                if state['theme'] == expected[index] and color_queries:
                    observed.append(state['theme'])
                    index += 1
                    if index < 3:
                        mode = 'dark' if index == 1 else 'light'
                        next_notice = time.monotonic() + .15
        assert observed == expected, f'{name}: observed {observed}; query count {color_queries}; enabled={reports_enabled}'
        assert reports_enabled, f'{name}: did not subscribe to Mode 2031'
        assert color_queries >= 3, f'{name}: did not re-query colors'
        print(f'PASS actual {name} TUI automatic Exposed → Shady → Exposed via Mode 2031 / OSC colors ({color_queries} queries)', flush=True)
    finally:
        status.with_suffix('.ansi').write_bytes(transcript)
        if child.poll() is None:
            os.killpg(child.pid, signal.SIGTERM)
            try:
                child.wait(timeout=5)
            except subprocess.TimeoutExpired:
                os.killpg(child.pid, signal.SIGKILL)
                child.wait(timeout=5)
        os.close(master)

with tempfile.TemporaryDirectory(prefix='exposed-terminal-') as temp:
    temp = Path(temp)
    env = {k: v for k, v in os.environ.items() if not k.startswith('HERDR_') and k not in ['PI_SESSION_FILE', 'PI_SESSION_ID']}
    env.update(TERM='xterm-256color', COLORTERM='truecolor', COLORFGBG='0;15', PI_OFFLINE='1')
    state = temp / 'nvim.json'
    lua = f'vim.fn.timer_start(100,function() vim.fn.writefile({{vim.json.encode({{theme=vim.g.colors_name}})}},{json.dumps(str(state))}) end,{{["repeat"]=-1}})'
    exercise('Neovim', ['nvim', '--cmd', 'let g:coc_start_at_startup=0', '-c', 'lua ' + lua], env, state)
    agent = temp / 'agent'
    (agent / 'themes').mkdir(parents=True)
    for name in ['exposed', 'shady']:
        (agent / 'themes' / f'{name}.json').symlink_to(ROOT / f'.pi/agent/themes/{name}.json')
    (agent / 'settings.json').write_text(json.dumps({'theme': 'exposed/shady', 'quietStartup': True, 'packages': []}))
    probe = temp / 'probe.ts'
    probe.write_text(PROBE)
    env.update(PI_CODING_AGENT_DIR=str(agent), EXPOSED_PROBE_FILE=str(temp / 'pi.json'))
    exercise('Pi', ['pi', '--no-approve', '--no-extensions', '--no-skills', '--no-prompt-templates', '--no-context-files',
                    '--extension', str(probe), '--provider', 'exposed-probe', '--model', 'idle', '--thinking', 'off'], env, temp / 'pi.json')
