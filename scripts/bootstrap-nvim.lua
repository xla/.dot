-- Invoked by setup-macos.sh after normal config/plugin loading. Never quit
-- before asynchronous minpac/parser jobs finish, and propagate errors to Bash.
local function bootstrap()
  assert(vim.fn.has('nvim-0.12') == 1, 'nvim-treesitter main requires Neovim >= 0.12')
  assert(vim.v.errmsg == '', 'Neovim startup failed: ' .. vim.v.errmsg)
  local stage = vim.env.DOTFILES_NVIM_STAGE
  if stage == 'plugins' then
    vim.fn.PackInit()
    assert(vim.fn.exists('*minpac#update') == 1, 'minpac failed to load')
    vim.g.dotfiles_plugins_done = false
    vim.fn['minpac#update']('', { ['do'] = 'let g:dotfiles_plugins_done = v:true' })
    assert(vim.wait(900000, function()
      return vim.g.dotfiles_plugins_done == true
    end, 100), 'Timed out installing Neovim plugins')
    for name, plugin in pairs(vim.g['minpac#pluglist']) do
      assert(plugin.stat.errcode == 0,
        'Plugin install failed: ' .. name .. '\n' .. table.concat(plugin.stat.lines, '\n'))
      assert(vim.fn.isdirectory(plugin.dir) == 1, 'Plugin missing: ' .. name)
    end
  elseif stage == 'editor' then
    assert(vim.fn.exists(':CocInstall') == 2, 'coc.nvim failed to load')
    assert(vim.fn.exists(':FuzzyOpen') == 2, 'neovim-fuzzy failed to load')
    assert(vim.fn.exists(':Rg') == 2, 'vim-ripgrep failed to load')

    -- CoC discovers npm dependencies in its extensions/package.json. Installing
    -- here avoids the interactive CocInstall UI and keeps an existing manifest
    -- (including any additional user extensions) intact.
    local extensions = vim.fn['coc#util#get_data_home']() .. '/extensions'
    vim.fn.mkdir(extensions, 'p')
    if vim.fn.filereadable(extensions .. '/package.json') == 0 then
      vim.fn.writefile({ '{"private":true,"dependencies":{}}' }, extensions .. '/package.json')
    end
    local command = { 'npm', '--prefix', extensions, 'install', '--save', '--save-exact',
      '--omit=dev', '--no-audit', '--no-fund' }
    for _, extension in ipairs(vim.g.coc_global_extensions) do
      table.insert(command, extension .. '@latest')
    end
    local result = vim.system(command, { text = true }):wait(900000)
    if result.stdout and result.stdout ~= '' then print(result.stdout) end
    assert(result.code == 0, 'CoC extension installation failed:\n' .. (result.stderr or ''))
    for _, extension in ipairs(vim.g.coc_global_extensions) do
      assert(vim.fn.filereadable(extensions .. '/node_modules/' .. extension .. '/package.json') == 1,
        'CoC extension missing: ' .. extension)
    end

    local treesitter = require('nvim-treesitter')
    -- install skips existing parsers; update skips missing ones. Wait for both
    -- so fresh installs and reruns match the newly updated plugin's revisions.
    treesitter.install(vim.g.dotfiles_treesitter_languages):wait(900000)
    treesitter.update(vim.g.dotfiles_treesitter_languages):wait(900000)
    for _, lang in ipairs(vim.g.dotfiles_treesitter_languages) do
      local loaded, err = pcall(vim.treesitter.language.add, lang)
      assert(loaded and err, 'Parser install failed for ' .. lang .. ': ' .. tostring(err))
      -- Check queries as well as the shared library (ABI/version mismatches).
      vim.treesitter.query.get(lang, 'highlights')
    end
    print('Neovim plugins, CoC extensions and parsers are ready.')
  else
    error('Unknown bootstrap stage: ' .. tostring(stage))
  end
end

local ok, err = xpcall(bootstrap, debug.traceback)
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd('cquit 1')
else
  vim.cmd('qall!')
end
