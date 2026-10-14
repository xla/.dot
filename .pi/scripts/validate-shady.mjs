// Usage: node .pi/scripts/validate-shady.mjs <pi-package-dir> [--installed] [--nvim] [--preview]
// Uses the installed Pi implementation; does not contact a model or change settings.
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { readFileSync, realpathSync, mkdtempSync, rmSync } from 'node:fs';
import { homedir, tmpdir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const packageDir = process.argv[2];
assert(packageDir && !packageDir.startsWith('--'), 'Pass the installed pi-coding-agent package directory');
const flags = new Set(process.argv.slice(3));
for (const flag of flags) assert(['--installed', '--nvim', '--preview'].includes(flag), `Unknown flag: ${flag}`);
const repo = resolve(dirname(fileURLToPath(import.meta.url)), '../..');
const themePath = join(repo, '.pi/agent/themes/shady.json');
const load = (path) => import(pathToFileURL(join(resolve(packageDir), path)).href);
const pi = await load('dist/modes/interactive/theme/theme.js');
const { validateThemeJson } = await load('dist/modes/interactive/theme/theme-json.js');
const { Markdown, visibleWidth, colorToHex } = await load('node_modules/@earendil-works/pi-tui/dist/index.js');
const json = JSON.parse(readFileSync(themePath, 'utf8'));
const schema = JSON.parse(readFileSync(join(packageDir, 'dist/modes/interactive/theme/theme-schema.json'), 'utf8'));
validateThemeJson(themePath, json);
assert.deepEqual(Object.keys(json.colors).sort(), Object.keys(schema.properties.colors.properties).sort(), 'Cover every current Pi color role');
assert.equal(json.name, 'shady');
assert.equal(json.appearance, 'dark');
assert.throws(() => validateThemeJson('negative test', { ...json, colors: {} }));
pi.setThemeJsonValidator(validateThemeJson);

// Expected values taken from the live cterm highlights, not Vim's unrelated GUI defaults.
const mappings = {
  text: ['Normal', 251], userMessageText: ['Normal', 251], toolOutput: ['Normal', 251],
  syntaxComment: ['Comment', 239], syntaxKeyword: ['Keyword', 253],
  syntaxFunction: ['Function', 248], syntaxVariable: ['Identifier', 109],
  syntaxString: ['String', 245], syntaxNumber: ['Number', 193], syntaxType: ['Type', 109],
  syntaxOperator: ['Operator', 248], syntaxPunctuation: ['Delimiter', 245],
  mdHeading: ['markdownH1', 254], mdCode: ['markdownCode', 246],
  mdCodeBlock: ['markdownCodeBlock', 246], mdCodeBlockBorder: ['markdownCodeDelimiter', 238],
  mdQuote: ['markdownBlockquote', 242], mdHr: ['markdownRule', 238],
  mdListBullet: ['markdownListMarker', 245], selectedBg: ['Visual', 235, 'ctermbg'],
};
const colors = pi.loadThemeFromPath(themePath, 'truecolor').colors;
for (const [token, [, index]] of Object.entries(mappings)) {
  assert.deepEqual(colors[token], { kind: 'indexed', index }, token);
}
const palette = readFileSync(join(repo, '.config/ghostty/themes/shady'), 'utf8');
const paletteHex = (index) => {
  const match = palette.match(new RegExp(`^palette = ${index}=(#[a-fA-F0-9]{6})$`, 'm'));
  assert(match, `Terminal palette color ${index} exists`);
  return match[1].toLowerCase();
};
for (const [token, index] of Object.entries({ error: 9, warning: 11, success: 10, toolDiffAdded: 10, toolDiffRemoved: 9, mdLink: 14, mdLinkUrl: 14 })) {
  assert.equal(colorToHex(colors[token]), paletteHex(index), token);
}
assert.equal(json.colors.customMessageBg, 'terminalBg');
assert.equal(json.vars.terminalBg, '');
assert.equal(json.colors.toolPendingBg, json.colors.toolSuccessBg);
assert.equal(json.colors.toolSuccessBg, json.colors.toolErrorBg);
for (const token of ['accent', 'border', 'borderAccent', 'borderMuted', 'thinkingOff', 'thinkingMinimal', 'thinkingLow', 'thinkingMedium', 'thinkingHigh', 'thinkingXhigh', 'thinkingMax']) {
  const hex = colorToHex(colors[token]);
  assert.equal(hex.slice(1, 3), hex.slice(3, 5), `${token} is grayscale`);
  assert.equal(hex.slice(3, 5), hex.slice(5, 7), `${token} is grayscale`);
}

if (flags.has('--nvim')) {
  const temp = mkdtempSync(join(tmpdir(), 'pi-shady-nvim-'));
  try {
    const outputPath = join(temp, 'highlights.json');
    const groups = [...new Set([...Object.values(mappings).map(([group]) => group), 'Error', 'Warning', 'DiffAdd', 'diffRemoved', 'markdownLink'])];
    const lua = `local out={name=vim.g.colors_name,truecolor=vim.o.termguicolors,groups={}}; for _,g in ipairs(vim.json.decode(${JSON.stringify(JSON.stringify(groups))})) do out.groups[g]=vim.api.nvim_get_hl(0,{name=g,link=false}) end; vim.fn.writefile({vim.json.encode(out)},${JSON.stringify(outputPath)})`;
    execFileSync('nvim', ['--headless', '-c', `lua ${lua}`, '-c', 'qa!'], { timeout: 20000, stdio: 'pipe' });
    const live = JSON.parse(readFileSync(outputPath, 'utf8'));
    assert.equal(live.name, 'shady');
    assert.equal(live.truecolor, false);
    for (const [token, [group, index, field = 'ctermfg']] of Object.entries(mappings)) {
      assert.equal(live.groups[group][field], index, `${token} matches live Neovim ${group}`);
    }
    for (const [group, index] of Object.entries({ Error: 9, Warning: 11, DiffAdd: 10, diffRemoved: 9, markdownLink: 14 })) {
      assert.equal(live.groups[group].ctermfg, index, group);
    }
    console.log(`PASS live Neovim: ${Object.keys(mappings).length} role mappings and 5 ANSI roles`);
  } finally { rmSync(temp, { recursive: true, force: true }); }
}

const sample = '# Shady\n\nBody text, **bold**, *italic*, `inline code`, and [a link](https://example.com).\n\n> A quiet quotation.\n\n- A grayscale bullet\n\n```typescript\n// A dim comment\nfunction greet(name: string) {\n  const count = 42;\n  return "hello";\n}\n```\n\n---';
for (const mode of ['truecolor', '256color']) {
  const theme = pi.loadThemeFromPath(themePath, mode);
  assert.equal(theme.getColorMode(), mode);
  for (const token of Object.keys(json.colors)) {
    const rendered = token.endsWith('Bg') ? theme.bg(token, 'sample') : theme.fg(token, 'sample');
    assert(rendered.includes('sample'), token);
    assert(!rendered.includes('undefined'), token);
  }
  assert.equal(theme.getFgAnsi('text'), '\x1b[38;5;251m');
  assert.equal(theme.getBgAnsi('customMessageBg'), '\x1b[49m');
  pi.setThemeInstance(theme);
  for (const width of [32, 80, 120]) {
    const lines = new Markdown(sample, 0, 0, pi.getMarkdownTheme()).render(width);
    assert(lines.length > 10);
    for (const line of lines) assert(visibleWidth(line) <= width, `Markdown fits ${width} columns`);
    const rendered = lines.join('\n');
    assert(rendered.includes(theme.getFgAnsi('mdHeading')));
    assert(rendered.includes(theme.getFgAnsi('syntaxComment')));
    assert(rendered.includes(theme.getFgAnsi('syntaxNumber')));
    if (flags.has('--preview') && mode === 'truecolor' && width === 80) console.log(rendered);
  }
}
console.log(`PASS schema, ${Object.keys(json.colors).length} color roles, palette fidelity, and Markdown at 32/80/120 columns in both color modes`);

if (flags.has('--installed')) {
  const agentDir = process.env.PI_CODING_AGENT_DIR || join(homedir(), '.pi/agent');
  assert.equal(realpathSync(join(agentDir, 'themes/shady.json')), realpathSync(themePath));
  const { SettingsManager } = await load('dist/core/settings-manager.js');
  const { DefaultResourceLoader } = await load('dist/core/resource-loader.js');
  const settingsManager = SettingsManager.create(repo, agentDir);
  assert.equal(settingsManager.getTheme(), 'shady');
  const loader = new DefaultResourceLoader({ cwd: repo, agentDir, settingsManager, noExtensions: true, noSkills: true, noPromptTemplates: true, noContextFiles: true });
  await loader.reload();
  const result = loader.getThemes();
  assert.deepEqual(result.diagnostics, [], 'Theme discovery has no errors or collisions');
  assert.equal(result.themes.filter((t) => t.name === 'shady').length, 1);
  pi.setRegisteredThemes(result.themes);
  assert.equal(pi.getAvailableThemes().filter((name) => name === 'shady').length, 1);
  assert.deepEqual(pi.setTheme(settingsManager.getTheme()), { success: true });
  assert.equal(pi.theme.name, 'shady', 'No silent fallback');
  assert.deepEqual(pi.getThemeExportColors('shady'), { pageBg: '#000000', cardBg: '#121212', infoBg: '#262626' });
  const htmlColors = pi.getResolvedThemeColors('shady');
  assert.equal(htmlColors.text, '#c6c6c6');
  assert.equal(htmlColors.error, paletteHex(9));
  assert.equal(htmlColors.mdLink, paletteHex(14));
  console.log('PASS installation, effective settings, real resource discovery, unique selector entry, activation without fallback, and HTML colors');
}
