" must be first to change other options as side effect
set nocompatible

set shell=/bin/sh

filetype off
" force reload of ftdetect files
filetype plugin indent on

set mouse=a
" hide buffers instead of closing them
set hidden
" use correct amount of spaces for a tab in insert mode
set expandtab
" a tab is two spaces
set tabstop=2
set softtabstop=2
" number of spaces to use for autoindenting
set shiftwidth=2
" use multiple of shiftwidth when indenting with '<' and '>'
set shiftround
" avoid modeline vulnerabilities
set modelines=0
" allow backspacing over everything in insert mode
set backspace=indent,eol,start
" insert tabs on the start of a line according to shiftwidth, not tabstop
set smarttab
" copies indentation from previous line
set autoindent
" copy the previous indentation on autoindenting
set copyindent
" always show line numbers
set number
" set show matching parenthesis
set showmatch
" ignore case when searching
set ignorecase
" ignore case if search pattern is all lowercase, case-sensitive otherwise
set smartcase
" highlight search terms
set hlsearch
" show search matches as you type
set incsearch
" remember more commands and search history
set history=1000
" use many muchos levels of undo
set undolevels=1000
set wildignore+=*.swp,*.bak,*.pyc,*.class,.git,node_modules/**,**Godeps
set wildignore+=*Godeps
" change the terminal's title
set title
" don't beep
set visualbell
" don't beep
set noerrorbells
" prevent auto indentation when pasting
set pastetoggle=<F2>
" font family & size
set guifont=Inconsolata:h18
" use utf-8 everywhere
set encoding=utf-8
" minimal lines to kepp above and below screen
set scrolloff=3
" display the mode you're in.
set showmode
" display incomplete commands.
set showcmd
" enhanced completion
set wildmenu
" enhanced completion
set wildmode=list:longest
" disable folding
set nofoldenable    
" highlight the line of the cursor
set cursorline
" smooth and fast redrawing
set ttyfast 
" show line and column info
set ruler
" wrap text if longer than window width
set wrap
" max text insertion width before breakage
set textwidth=79
" description of automatic formatting
set formatoptions=qrn1
" auto-save the file on different commands
set autowriteall
" Suffixes that get lower priority when doing tab completion for filenames.
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc,.pyc,.class,.jar 
" EOL for current buffer
set fileformat=unix
" list of EOL formats to try
set fileformats=unix,dos,mac 
" store history information
set viminfo=!,'50,\"1000,:150,n~/.vim/viminfo

" autoread changed files
set autoread
au FocusGained * :checktime

"" backup/swap/undo
" enable backups
set backup 
" backup file directory
set backupdir=~/.vim/tmp/backup/
" swap file directory
set directory=~/.vim/tmp/swap/
" undo file directory
if exists('+undodir')
  set undodir=~/.vim/tmp/undo/ 
endif
" backup before overwritting
set writebackup

set background=dark

" show status line
set laststatus=2
set statusline=

" file name
set statusline +=%t
" modified flag
set statusline +=\ %#todo#%m%*
" right aligned from here
set statusline +=%=
" current line/total lines and column
set statusline +=[%3l/%-3L\|%-2c]
" file type
set statusline +=\ %Y

set scl=yes

set completeopt-=longest
set completeopt-=menu
set completeopt-=preview

" Set python paths explicitly under macOS.
if has('macunix')
  let g:python2_host_prog = '/usr/local/bin/python'
  let g:python3_host_prog = '/usr/local/bin/python3'
endif

" switch syntax highlighting on, when the terminal has colors
if &t_Co > 2 || has("gui_running")
  syntax enable
  syntax sync fromstart
endif

" change the mapleader from \ to ,
let mapleader=","

" spare extra modifier key for commands
nnoremap ; :

" quickly edit/reload the vimrc file
nmap <silent> <leader>ev :e $MYVIMRC<cr>
nmap <silent> <leader>sv :so $MYVIMRC<cr>

" force home row usage
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>

" step line by line even when text is wrapped
nnoremap j gj
nnoremap k gk

" clear search highlights
nmap <silent> <leader><space> :nohlsearch<cr>

" use w!! to use sudo after opening
cmap w!! w !sudo tee % > /dev/null

" prevent vim regex handling
nnoremap / /\v
vnoremap / /\v

" shortcut to rapidly toggle `set list`
nmap <leader>l :set list!<cr>

" remap help file
inoremap <F1> <ESC>
" escape from insert mode
inoremap jk <ESC>

" quickfix navigation
map <C-n> :cnext<cr>
map <C-m> :cprevious<cr>
nnoremap <leader>a :cclose<cr>

" map make
nmap <leader>m :make!<cr>

" remember cursor position
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

" Per file-type indentation
au FileType                    haskell     setlocal sts=4 sw=4 expandtab
au FileType                    javascript  setlocal fo=cqt sts=2 sw=2 tw=80 wm=0 expandtab
au FileType                    css         setlocal ts=2  sw=2 noexpandtab
au FileType                    go          setlocal ts=4  sw=4 noexpandtab
au BufNewFile,BufRead,FileType *.go        setlocal ts=4  sw=4 noexpandtab
au FileType                    c,cpp       setlocal ts=8  sw=8 noexpandtab
au FileType                    lua         setlocal       sw=4 expandtab
au FileType                    sh,zsh      setlocal ts=2  sw=2 noexpandtab
au FileType                    vim,ruby    setlocal sts=2 sw=2 expandtab
au FileType                    help        setlocal ts=4  sw=4 noexpandtab
au FileType                    txt         setlocal noai nocin nosi inde= wrap linebreak
au FileType                    pandoc      setlocal nonumber
au FileType                    markdown    setlocal nonumber
au FileType                    fountain    setlocal nonumber noai nocin nosi inde= wrap linebreak

if has("nvim")
  call plug#begin()

  Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
  Plug 'cloudhead/neovim-fuzzy'
  Plug 'cloudhead/shady.vim'
  Plug 'exu/pgsql.vim'
  Plug 'fatih/vim-go'
  Plug 'frigoeu/psc-ide-vim'
  Plug 'hwayne/tla.vim'
  Plug 'hail2u/vim-css3-syntax'
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
  Plug 'junegunn/fzf.vim'
  Plug 'mileszs/ack.vim'
  Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }
  Plug 'pbogut/deoplete-elm'
  Plug 'purescript-contrib/purescript-vim'
  Plug 'sbdchd/neoformat'
  Plug 'shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  Plug 'theJian/elm.vim'
  Plug 'tpope/vim-commentary'
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-markdown'
  Plug 'vim-syntastic/syntastic'
  Plug 'w0rp/ale'
  Plug 'zchee/deoplete-go', {'build': {'unix': 'make'}}

  Plug 'pangloss/vim-javascript'
  Plug 'maxmellon/vim-jsx-pretty'
  Plug 'ludovicchabant/vim-gutentags'
  Plug 'styled-components/vim-styled-components', { 'branch': 'main' }
  Plug 'skywind3000/asyncrun.vim'

  call plug#end()
endif

try
  colorscheme shady
catch
endtry

" ale
let g:ale_enabled = 0
let g:ale_fix_on_save = 1
let g:ale_lint_on_enter = 0
let g:ale_lint_on_text_changed = "never"
let g:ale_open_list = 1
let g:ale_set_highlights = 0
let g:ale_set_loclist = 0
let g:ale_set_quickfix = 1
let g:ale_sign_column_always = 1
let g:ale_sign_error = "xx"
let g:ale_sign_warning = "--"
let g:ale_parse_makefile = 1
let g:ale_lint_on_enter = 0 " Less distracting when opening a new file

let g:ale_fixers = {
  \ 'javascript': [ 'eslint', 'prettier' ],
  \ }

let g:ale_linters = {
  \ 'css': [ 'stylelint' ],
  \ 'go': [ 'gometalinter' ],
  \ 'javascript': [ 'eslint', 'stylelint' ],
  \ 'js': [ 'eslint', 'stylelint' ],
  \ }
let g:ale_linter_aliases = {
  \ 'javascript': 'css',
  \ 'jsx': 'css',
  \ }

let g:ale_go_gometalinter_options = '
  \ --aggregate
  \ --concurrency 6
  \ --cyclo-over 20
  \ --deadline 500ms
  \ --enable-all
  \ --fast
  \ --disable vetshadow
  \ --disable dupl
  \ --sort line
  \ --tests
  \ --vendor
  \ '

" au FileType c let g:ale_enabled = 1
au FileType go let g:ale_enabled = 1
au FileType javascript let g:ale_enabled = 1
au FileType lua let g:ale_enabled = 1

" LanguageClient
let g:LanguageClient_serverCommands = {
    \ "cpp": ["cquery", "--log-file=/tmp/cq.log"],
    \ "c": ["cquery", "--log-file=/tmp/cq.log"],
    \ }

let g:LanguageClient_loadSettings = 1
" Use an absolute configuration path if you want system-wide settings
let g:LanguageClient_settingsPath = '/home/xla/.config/nvim/settings.json'
set completefunc=LanguageClient#complete
set formatexpr=LanguageClient_textDocument_rangeFormatting()

au FileType c nnoremap <silent> gh :call LanguageClient#textDocument_hover()<CR>
au FileType c nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
au FileType c nnoremap <silent> gr :call LanguageClient#textDocument_references()<CR>
au FileType c nnoremap <silent> gs :call LanguageClient#textDocument_documentSymbol()<CR>
au FileType c nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>

" deoplete
if has("nvim")
  call deoplete#enable()
  call deoplete#custom#option('auto_complete', 0)
  call deoplete#custom#option('ignore_sources', {'_': [ "around", "buffer", "dictionary", "file", "member", "omni", "tag" ] })
  call deoplete#custom#option('max_list', 20)

  let g:deoplete#sources#go#package_dot = 1
  let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']

  inoremap <silent><expr> <TAB>
    \ pumvisible() ? "\<C-n>" :
    \ <SID>check_back_space() ? "\<TAB>" :
    \ deoplete#manual_complete()

  inoremap <silent><expr> <C-n>
    \ pumvisible() ? "\<C-n>" :
    \ <SID>check_back_space() ? "\<TAB>" :
    \ deoplete#manual_complete()

  function! s:check_back_space()
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~ '\s'
  endfunction
endif

" fuzzy
nnoremap <leader>o :FuzzyOpen<cr>
nnoremap <leader>f :FuzzyGrep<cr>

" goyo
let g:goyo_width = 100
let g:goyo_height = "90%"

" lua
let g:lua_complete_omni = 1

" neoformat
augroup fmt
  autocmd!
  autocmd BufWritePre *.elm undojoin | Neoformat
augroup END

" purescript
let g:psc_ide_syntastic_mode = 1

autocmd FileType purescript nmap <leader>b :Prebuild!<cr>
autocmd Filetype purescript autocmd BufWritePost !purty --write %

" vim-commentary
nmap <C-_> <Plug>CommentaryLine
xmap <C-_> <Plug>Commentary

" vim-go
autocmd FileType go nmap <leader>b <Plug>(go-build)
autocmd FileType go nmap <leader>c <Plug>(go-coverage-toggle)
autocmd FileType go nmap <leader>r <Plug>(go-run)
autocmd FileType go nmap <leader>t <Plug>(go-test)

let g:go_addtags_transform = "snakecase"
let g:go_auto_type_info = 1
let g:go_fmt_command = "goimports"
let g:go_fmt_experimental = 1
let g:go_list_type = "quickfix"

" custom highlight
hi User1 ctermbg=black ctermfg=red guibg=black guifg=red

if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

" Syntastic
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" TODO list
command! Todo Ack! 'TODO'
command! TodoLocal Ack! 'TODO' %

nnoremap <leader>tg :Todo<cr>
nnoremap <leader>tl :TodoLocal<cr>
