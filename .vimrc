" ---------------------------------------------------------------------------
" Compatibility
" ---------------------------------------------------------------------------

" Normally `:set nocompatible` is not needed because it is done automatically
" when a vimrc is found, but keep the explicit guard for safety.
if &compatible
  set nocompatible
endif

" Enable filetype detection, filetype plugins and indentation rules.
filetype plugin indent on


" ---------------------------------------------------------------------------
" Core editor behaviour
" ---------------------------------------------------------------------------

set mouse=a
" allow switching buffers without forcing writes
set hidden

" use spaces instead of tab characters by default
set expandtab
" default tab width for general web-oriented editing
set tabstop=2
set softtabstop=2
" indentation width for >> << and autoindent
set shiftwidth=2
" round indent operations to multiples of shiftwidth
set shiftround

" disable modelines for safety
set modelines=0
" allow backspacing over indentation, line breaks and insert start
set backspace=indent,eol,start
" preserve indentation from previous line
set autoindent
" copy existing indentation structure where possible
set copyindent

" always show absolute line numbers
set number

" ignore case when searching...
set ignorecase
" ...unless the pattern contains uppercase characters
set smartcase
" keep search matches highlighted
set hlsearch
" show matches while typing the search pattern
set incsearch

" keep a few context lines visible above and below the cursor
set scrolloff=3

" disable code folding by default to reduce UI clutter
set nofoldenable

" automatically write buffers on commands like :next, :make, etc.
set autowriteall

" default to unix line endings
set fileformat=unix
" formats to try when reading files
set fileformats=unix,dos,mac

" always reserve sign column to avoid text shifting from diagnostics
set signcolumn=yes

" shorter update time improves CursorHold and diagnostic responsiveness
set updatetime=300

" horizontal splits open below current window
set splitbelow

" detect file changes on disk when possible
set autoread

" keep terminal color handling in the old path because shady depends on it
set notermguicolors

" do not highlight the current line
set nocursorline

" no wrapping globally for code-oriented workflow
set nowrap

" do not enter paste mode by default
set nopaste


" ---------------------------------------------------------------------------
" Persistence / recovery
" ---------------------------------------------------------------------------

" keep swap files in one dedicated location
set directory=~/.vim/tmp/swap//

" keep persistent undo history if supported
if exists('+undodir')
  set undofile
  set undodir=~/.vim/tmp/undo//
endif

" do not create backup files next to edited files
set nobackup
set nowritebackup


" ---------------------------------------------------------------------------
" Status line
" ---------------------------------------------------------------------------

" always show the statusline
set laststatus=2
set statusline=

" file name
set statusline+=%t
" modified flag
set statusline+=\ %m
" right align from here
set statusline+=%=
" current line / total lines and column
set statusline+=[%3l/%-3L\|%-2c]
" file type
set statusline+=\ %Y


" ---------------------------------------------------------------------------
" Python host under macOS
" ---------------------------------------------------------------------------

" Ensure Neovim can find Homebrew tools and the pinned Python host.
if has('macunix')
  let s:brew_prefix = isdirectory('/opt/homebrew') ? '/opt/homebrew' : '/usr/local'
  let $PATH = expand('~/.local/bin') . ':' . expand('~/.cargo/bin') . ':' . s:brew_prefix . '/bin:' . $PATH
  let g:python3_host_prog = expand('$HOME/.venvs/neovim/bin/python')
endif


" ---------------------------------------------------------------------------
" Syntax / colors
" ---------------------------------------------------------------------------

" enable syntax highlighting
syntax enable

" leader key
let mapleader=","

" load shady if available
try
  colorscheme shady
  set background=dark
catch
endtry


" ---------------------------------------------------------------------------
" General mappings
" ---------------------------------------------------------------------------

" spare an easy key for command-line mode
nnoremap ; :

" quickly edit / reload vimrc
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :source $MYVIMRC<CR>

" force home-row navigation discipline
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>

" clear search highlight
nmap <silent> <leader><space> :nohlsearch<CR>

" allow `:w!!` to write with sudo after opening a file normally
cmap w!! w !sudo tee % > /dev/null

" default searches to very magic regex mode
nnoremap / /\v
vnoremap / /\v

" toggle invisible characters
nmap <leader>l :set list!<CR>

" F1 is more annoying than useful
inoremap <F1> <ESC>

" quick escape from insert mode
inoremap jk <ESC>

" preserve wrapped-line movement muscle memory
" this matters mainly in markdown/text buffers where wrap is enabled locally
nnoremap j gj
nnoremap k gk


" ---------------------------------------------------------------------------
" Quickfix / location list
" ---------------------------------------------------------------------------

" next / previous entry in location list
nnoremap <C-n> :lnext<CR>
nnoremap <C-m> :lprevious<CR>

" toggle location list
nnoremap <leader>a :LToggle<CR>

" keep existing make shortcut
nmap <leader>m :make!<CR>


" ---------------------------------------------------------------------------
" Plugin management with minpac
" ---------------------------------------------------------------------------

" Bootstrap minpac itself on a fresh machine.
if empty(glob('~/.vim/pack/minpac/opt/minpac/autoload/minpac.vim'))
  echohl WarningMsg
  echom 'minpac not found: clone https://github.com/k-takata/minpac.git to ~/.vim/pack/minpac/opt/minpac'
  echohl None
endif

" Load minpac only if present.
silent! packadd minpac

function! PackInit() abort
  " Autoload functions do not exist until first called; check the file instead.
  if empty(glob('~/.vim/pack/minpac/opt/minpac/autoload/minpac.vim'))
    return
  endif

  call minpac#init({'dir': expand('~/.vim'),
        \ 'progress_open': get(g:, 'dotfiles_bootstrap', 0) ? 'none' : 'horizontal'})
  call minpac#add('k-takata/minpac', {'type': 'opt'})

  " comments
  call minpac#add('tpope/vim-commentary')

  " fuzzy navigation / grep
  call minpac#add('cloudhead/neovim-fuzzy')
  call minpac#add('jremmen/vim-ripgrep')

  " language intelligence / completion / diagnostics
  call minpac#add('neoclide/coc.nvim', {'branch': 'release'})

  " light syntax / filetype support where still useful
  call minpac#add('cespare/vim-toml')
  call minpac#add('dag/vim-fish')

  " theme
  call minpac#add('cloudhead/shady.vim')

  " tree-sitter
  call minpac#add('nvim-treesitter/nvim-treesitter', {'branch': 'main',
        \ 'do': get(g:, 'dotfiles_bootstrap', 0) ? '' : ':TSUpdate'})
endfunction

command! PackClean  call PackInit() | call minpac#clean()
command! PackStatus call PackInit() | call minpac#status()
command! PackUpdate call PackInit() | call minpac#update('', {'do': 'call minpac#status()'})

" plugins need to be added to runtimepath before helptags can be generated
packloadall
silent! helptags ALL


" ---------------------------------------------------------------------------
" Tree-sitter
" ---------------------------------------------------------------------------

" Tree-sitter owns syntax parsing and highlighting for primary languages.
" Guard startup so Neovim still boots before the plugin is installed.
lua << EOF
vim.g.dotfiles_treesitter_languages = {
  'go', 'gomod', 'gosum', 'gowork', 'rust', 'javascript', 'typescript',
  'tsx', 'lua', 'vim', 'vimdoc', 'query', 'markdown', 'markdown_inline',
  'toml', 'html', 'css', 'svelte',
}
local ok, treesitter = pcall(require, 'nvim-treesitter')
if ok then
  treesitter.setup {}
  local group = vim.api.nvim_create_augroup('xla_treesitter', { clear = true })
  vim.api.nvim_create_autocmd('FileType', {
    group = group,
    callback = function(args)
      local lang = vim.treesitter.language.get_lang(vim.bo[args.buf].filetype)
      if vim.tbl_contains(vim.g.dotfiles_treesitter_languages, lang) then
        -- Missing parsers must not break first startup, before setup runs.
        pcall(vim.treesitter.start, args.buf, lang)
      end
    end,
  })
end
EOF


" ---------------------------------------------------------------------------
" Fuzzy search / grep
" ---------------------------------------------------------------------------

" fuzzy file open / grep
nnoremap <leader>o :FuzzyOpen<CR>
nnoremap <leader>f :FuzzyGrep<CR>

" prefer ripgrep when available
if executable('rg')
  let g:ackprg = 'rg --vimgrep --no-heading'
  set grepprg=rg\ --vimgrep
endif

" TODO helpers
command! Todo Rg 'TODO'
command! TodoLocal Rg 'TODO' %

nnoremap <leader>tg :Todo<CR>
nnoremap <leader>tl :TodoLocal<CR>


" ---------------------------------------------------------------------------
" Commentary
" ---------------------------------------------------------------------------

" comment current line / selection
nmap <C-_> <Plug>CommentaryLine
xmap <C-_> <Plug>Commentary


" ---------------------------------------------------------------------------
" CoC
" ---------------------------------------------------------------------------

" Keep this list as the single source of truth for setup and future installs.
let g:coc_global_extensions = [
      \ 'coc-json', 'coc-eslint', 'coc-prettier', 'coc-yaml', 'coc-tsserver',
      \ 'coc-svelte', 'coc-lua', 'coc-rust-analyzer', 'coc-go', 'coc-snippets']

" helper for tab completion fallback
function! CheckBackspace() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1] =~# '\s'
endfunction

" use Tab to navigate completion menu, insert tab at whitespace,
" otherwise trigger completion
inoremap <silent><expr> <TAB>
      \ coc#pum#visible() ? coc#pum#next(1) :
      \ CheckBackspace() ? "\<Tab>" :
      \ coc#refresh()

" reverse direction through completion menu
inoremap <silent><expr> <S-TAB>
      \ coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"

" confirm completion with Enter when popup is visible
inoremap <silent><expr> <CR>
      \ coc#pum#visible() ? coc#pum#confirm()
      \ : "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" manual completion trigger
inoremap <silent><expr> <C-Space> coc#refresh()

" close preview window when completion finishes
autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif

" go-to mappings
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" hover / documentation
nnoremap <silent> K :call <SID>show_documentation()<CR>

" code actions
nmap <C-a> <Plug>(coc-codeaction)

function! s:show_documentation()
  if &filetype ==# 'vim'
    execute 'h ' . expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" explicit prettier hook if present in workspace
command! -nargs=0 Prettier :call CocAction('runCommand', 'prettier.formatFile')

" show diagnostics list
nnoremap <silent> <space>a :<C-u>CocList diagnostics<CR>


" ---------------------------------------------------------------------------
" Filetypes / autocmds
" ---------------------------------------------------------------------------

augroup xla_filetypes
  autocmd!

  " restore cursor position when reopening a file
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   execute "normal! g`\"" |
    \ endif

  " re-check file changes when focus returns or buffer is entered
  autocmd FocusGained,BufEnter * checktime

  " explicit filetype guards for formats that benefit from it
  autocmd BufNewFile,BufRead *.tsx,*.jsx setfiletype typescriptreact
  autocmd BufNewFile,BufRead *.svelte   setfiletype svelte
  autocmd BufNewFile,BufRead *.toml,Cargo.lock,Gopkg.lock,*/.cargo/config,*/.cargo/credentials,Pipfile setfiletype toml

  " Go uses tabs by convention; render them at standard Go width.
  autocmd FileType go setlocal noexpandtab tabstop=8 shiftwidth=8 softtabstop=0 nowrap
  autocmd FileType gomod setlocal noexpandtab tabstop=8 shiftwidth=8 softtabstop=0 nowrap
  autocmd FileType gosum setlocal noexpandtab tabstop=8 shiftwidth=8 softtabstop=0 nowrap
  autocmd FileType gowork setlocal noexpandtab tabstop=8 shiftwidth=8 softtabstop=0 nowrap

  " Use go test as the default make target for Go buffers.
  autocmd FileType go setlocal makeprg=go\ test\ ./...

  " rust uses 4 spaces and no wrap
  autocmd FileType rust setlocal tabstop=4 softtabstop=4 shiftwidth=4 expandtab nowrap
  " cargo check as the local make target for rust buffers
  autocmd FileType rust setlocal makeprg=cargo\ check

  " lua stays at 2 spaces to match common neovim config style
  autocmd FileType lua setlocal tabstop=2 softtabstop=2 shiftwidth=2 expandtab

  " prose-oriented buffers can wrap and drop line numbers
  autocmd FileType markdown setlocal wrap linebreak nonumber
  autocmd FileType text     setlocal wrap linebreak
augroup END

" ---------------------------------------------------------------------------
" Go
" ---------------------------------------------------------------------------
augroup xla_go
  autocmd!

  " Organize Go imports before writing.
  autocmd BufWritePre *.go silent! call CocAction('runCommand', 'editor.action.organizeImport')

  " Test navigation / generation
  autocmd FileType go nnoremap <buffer> <leader>gt :CocCommand go.test.toggle<CR>
  autocmd FileType go nnoremap <buffer> <leader>gF :CocCommand go.test.generate.file<CR>
  autocmd FileType go nnoremap <buffer> <leader>gf :CocCommand go.test.generate.function<CR>
  autocmd FileType go nnoremap <buffer> <leader>gE :CocCommand go.test.generate.exported<CR>

  " Interface implementation
  autocmd FileType go nnoremap <buffer> <leader>gi :CocCommand go.impl.cursor<CR>

  " Struct tags
  autocmd FileType go nnoremap <buffer> <leader>gj :CocCommand go.tags.add json<CR>
  autocmd FileType go nnoremap <buffer> <leader>gy :CocCommand go.tags.add yaml<CR>
  autocmd FileType go nnoremap <buffer> <leader>gx :CocCommand go.tags.clear<CR>

  " Module maintenance
  autocmd FileType go nnoremap <buffer> <leader>gm :CocCommand go.gopls.tidy<CR>
augroup END

command! GoLint !golangci-lint run ./...


" ---------------------------------------------------------------------------
" CoC diagnostic highlighting
" ---------------------------------------------------------------------------

" explicit sign colors
hi CocErrorSign    ctermfg=red    guibg=black guifg=red
hi CocWarningSign  ctermfg=yellow guibg=black guifg=yellow

" link virtual text / signs / floats to clearer groups
hi link CocErrorVirtualText    Error
hi link CocErrorSign           Error
hi      CocErrorHighlight      cterm=undercurl guisp=#B03060
hi link CocErrorFloat          Error

hi link CocWarningVirtualText  Warning
hi link CocWarningSign         Warning
hi      CocWarningHighlight    cterm=undercurl guisp=#FFE4B5

hi link CocInfoVirtualText     Identifier
hi link CocInfoSign            Identifier
hi      CocInfoHighlight       cterm=underline guisp=blue

hi link CocHintVirtualText     Comment
hi link CocHintSign            Comment
hi      CocHintHighlight       cterm=none guisp=blue
hi      CocUnusedHighlight     ctermfg=246 cterm=strikethrough

hi link CocCodeLens            Comment
hi link CocFloating            Pmenu


" ---------------------------------------------------------------------------
" Helper functions
" ---------------------------------------------------------------------------

command! LToggle call s:LListToggle()

function! s:LListToggle() abort
  let buffer_count_before = s:BufferCount()

  " location list cannot be closed if the cursor is in it,
  " so try closing twice
  silent! lclose
  silent! lclose

  if s:BufferCount() == buffer_count_before
    execute "silent! lopen 10"
  endif
endfunction

function! s:BufferCount() abort
  return len(filter(range(1, bufnr('$')), 'buflisted(v:val)'))
endfunction
