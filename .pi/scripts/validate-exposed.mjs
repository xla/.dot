// Usage: node .pi/scripts/validate-exposed.mjs <pi-package-dir> [--installed] [--preview]
// No model calls, settings changes, or changes to the caller's UI.
import assert from 'node:assert/strict';
import { readFileSync, realpathSync } from 'node:fs';
import { homedir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
const packageDir = process.argv[2];
assert(packageDir && !packageDir.startsWith('--'), 'Pass the installed Pi package directory');
const flags = new Set(process.argv.slice(3));
for (const flag of flags) assert(['--installed', '--preview'].includes(flag), `Unknown flag: ${flag}`);
const repo = resolve(dirname(fileURLToPath(import.meta.url)), '../..');
const path = join(repo, '.pi/agent/themes/exposed.json');
const load = relative => import(pathToFileURL(join(resolve(packageDir), relative)).href);
const pi = await load('dist/modes/interactive/theme/theme.js');
const { validateThemeJson } = await load('dist/modes/interactive/theme/theme-json.js');
const { Markdown, visibleWidth, colorToHex, parseColor } = await load('node_modules/@earendil-works/pi-tui/dist/index.js');
const json = JSON.parse(readFileSync(path, 'utf8'));
const schema = JSON.parse(readFileSync(join(packageDir, 'dist/modes/interactive/theme/theme-schema.json'), 'utf8'));
validateThemeJson(path, json);
pi.setThemeJsonValidator(validateThemeJson);
assert.equal(json.name, 'exposed');
assert.equal(json.appearance, 'light');
assert.deepEqual(Object.keys(json.colors).sort(), Object.keys(schema.properties.colors.properties).sort());
assert.throws(() => validateThemeJson('negative control', { ...json, colors: {} }));
const luminance = hex => {
  const a = [1, 3, 5].map(i => parseInt(hex.slice(i, i + 2), 16) / 255).map(v => v <= .04045 ? v / 12.92 : ((v + .055) / 1.055) ** 2.4);
  return a.reduce((sum, v, i) => sum + v * [.2126, .7152, .0722][i], 0);
};
const contrast = (a, b) => {
  const [lo, hi] = [luminance(a), luminance(b)].sort((a, b) => a - b);
  return (hi + .05) / (lo + .05);
};
const backgrounds = ['selectedBg', 'userMessageBg', 'customMessageBg', 'toolPendingBg', 'toolSuccessBg', 'toolErrorBg'];
const foregrounds = Object.keys(json.colors).filter(key => !backgrounds.includes(key) && !['searchMatchBg', 'searchMatchText'].includes(key));
let count = 0, minimum = Infinity;
const sample = '# Exposed\n\nReadable **bold**, *italic*, `code`, and [links](https://example.com).\n\n> Comments remain readable.\n\n- State cues use labels too.\n\n```typescript\n// Readable comment\nfunction greet(name: string) {\n  const count = 42;\n  return "hello";\n}\n```\n\n---';
for (const mode of ['truecolor', '256color']) {
  const t = pi.loadThemeFromPath(path, mode);
  assert.equal(t.getColorMode(), mode);
  for (const key of Object.keys(json.colors)) {
    assert.equal(t.colors[key].kind, 'indexed', 'Exact extended-palette colors in both modes');
    assert(t.colors[key].index >= 16, 'Never depend on mutable ANSI colors');
  }
  for (const fg of foregrounds) for (const bg of backgrounds) {
    const ratio = contrast(colorToHex(t.colors[fg]), colorToHex(t.colors[bg]));
    assert(ratio >= 4.5, `${mode} ${fg}/${bg}: ${ratio.toFixed(2)}:1`);
    minimum = Math.min(minimum, ratio); count++;
  }
  assert(contrast(colorToHex(t.colors.searchMatchText), colorToHex(t.colors.searchMatchBg)) >= 4.5);
  let previous = Infinity;
  for (const token of ['thinkingOff', 'thinkingMinimal', 'thinkingLow', 'thinkingMedium', 'thinkingHigh', 'thinkingXhigh', 'thinkingMax']) {
    const hex = colorToHex(t.colors[token]);
    assert.equal(hex.slice(1, 3), hex.slice(3, 5));
    assert.equal(hex.slice(3, 5), hex.slice(5, 7));
    assert(luminance(hex) < previous, 'Higher thinking levels have progressively darker neutral borders');
    previous = luminance(hex);
  }
  pi.setThemeInstance(t);
  for (const width of [32, 80, 120]) {
    const lines = new Markdown(sample, 0, 0, pi.getMarkdownTheme()).render(width);
    assert(lines.length > 10);
    for (const line of lines) assert(visibleWidth(line) <= width, `Fits ${width} columns`);
    const render = lines.join('\n');
    for (const token of ['mdHeading', 'syntaxComment', 'syntaxNumber', 'syntaxType', 'mdLink']) assert(render.includes(t.getFgAnsi(token)), token);
    if (flags.has('--preview') && mode === 'truecolor' && width === 80) console.log(render);
  }
}
assert(contrast('#eeeeee', '#ffffff') < 4.5, 'Negative control rejects unreadable text');
assert.deepEqual(pi.parseAutoThemeSetting('exposed/shady'), { lightTheme: 'exposed', darkTheme: 'shady' });
for (const appearance of ['light', 'dark', 'light']) {
  assert.equal(pi.resolveThemeSetting('exposed/shady', appearance), appearance === 'light' ? 'exposed' : 'shady');
}
console.log(`PASS all ${foregrounds.length} Exposed foreground roles across ${backgrounds.length} surfaces in both modes (${count} pairs; minimum ${minimum.toFixed(2)}:1), search, schema, and real Markdown/code at 32/80/120 columns`);
if (flags.has('--installed')) {
  const agentDir = process.env.PI_CODING_AGENT_DIR || join(homedir(), '.pi/agent');
  for (const name of ['exposed', 'shady']) assert.equal(realpathSync(join(agentDir, `themes/${name}.json`)), realpathSync(join(repo, `.pi/agent/themes/${name}.json`)));
  const { SettingsManager } = await load('dist/core/settings-manager.js');
  const { DefaultResourceLoader } = await load('dist/core/resource-loader.js');
  const settingsManager = SettingsManager.create(repo, agentDir);
  assert.equal(settingsManager.getThemeSetting(), 'exposed/shady');
  const loader = new DefaultResourceLoader({ cwd: repo, agentDir, settingsManager, noExtensions: true, noSkills: true, noPromptTemplates: true, noContextFiles: true });
  await loader.reload();
  const result = loader.getThemes();
  assert.deepEqual(result.diagnostics, []);
  pi.setRegisteredThemes(result.themes);
  for (const name of ['exposed', 'shady']) assert.equal(pi.getAvailableThemes().filter(n => n === name).length, 1);
  for (const appearance of ['light', 'dark', 'light']) {
    pi.setTerminalColors({ foreground: parseColor(appearance === 'light' ? '#303030' : '#ffffff'), background: parseColor(appearance === 'light' ? '#ffffff' : '#000000') });
    assert.equal(pi.getTerminalTheme(), appearance);
    const name = pi.resolveThemeSetting(settingsManager.getThemeSetting(), pi.getTerminalTheme());
    assert.deepEqual(pi.setTheme(name), { success: true });
    assert.equal(pi.theme.name, appearance === 'light' ? 'exposed' : 'shady', 'No silent fallback');
  }
  assert.deepEqual(pi.getThemeExportColors('exposed'), { pageBg: '#ffffff', cardBg: '#eeeeee', infoBg: '#dadada' });
  const html = pi.getResolvedThemeColors('exposed');
  assert.equal(html.text, '#303030'); assert.equal(html.error, '#87005f');
  console.log('PASS global installation, effective auto setting, unique discovery, light/dark/light activation, and HTML export');
}
