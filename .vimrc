" must be first to change other options as side effect
set nocompatible

set shell=/bin/sh

filetype off
" force reload of ftdetect files
filetype plugin indent on

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
" set cursorline
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
" modified flag
set statusline +=\ %1*%m
" file name
set statusline +=%*%t
" current line
set statusline +=\ [%2l/%L+%c]

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
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

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
nmap <leader>l :set list!<CR>

" remap help file
inoremap <F1> <ESC>
" escape from insert mode
inoremap jk <ESC>

" remember cursor position
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

" Per file-type indentation
au FileType haskell     setlocal sts=4 sw=4 expandtab
au FileType javascript  setlocal sts=4 sw=4 expandtab
au FileType css         setlocal ts=4  sw=4 noexpandtab
au FileType go          setlocal ts=4  sw=4 noexpandtab
au FileType c,cpp       setlocal       sw=4 noexpandtab
au FileType lua         setlocal       sw=2 expandtab
au FileType sh,zsh      setlocal ts=2  sw=2 noexpandtab
au FileType vim,ruby    setlocal sts=2 sw=2 expandtab
au FileType help        setlocal ts=4  sw=4 noexpandtab
au FileType txt         setlocal noai nocin nosi inde= wrap linebreak
au FileType pandoc      setlocal nonumber
au FileType markdown    setlocal nonumber
au FileType fountain    setlocal nonumber noai nocin nosi inde= wrap linebreak

if has("nvim")
  call plug#begin()

  Plug 'cloudhead/neovim-fuzzy'
  Plug 'cloudhead/shady.vim'
  Plug 'exu/pgsql.vim'
  Plug 'fatih/vim-go'
  Plug 'hail2u/vim-css3-syntax'
  Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }
  Plug 'shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  Plug 'tpope/vim-commentary'
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-markdown'
  Plug 'zchee/deoplete-go', {'build': {'unix': 'make'}}

  call plug#end()
endif

try
  colorscheme shady
catch
endtry

" Commentary
nmap <C-_>  <Plug>CommentaryLine
xmap <C-_>  <Plug>Commentar

" File navigation/search
nnoremap <Leader>o :FuzzyOpen<CR>
nnoremap <Leader>f :FuzzyGrep<CR>

" Go
" linting & vetting for go files
autocmd FileType go autocmd BufWritePost <buffer> GoVet
autocmd FileType go autocmd BufWritePost <buffer> GoLint

nmap <leader>b :GoBuild<cr>
nmap <leader>m :GoTest<cr>

let g:go_auto_type_info = 1
let g:go_fmt_command = "goimports"
let g:go_fmt_experimental = 1


" ----- parsonsmatt/intero-neovim -----

" Prefer starting Intero manually (faster startup times)
let g:intero_start_immediately = 0
" Use ALE (works even when not using Intero)
let g:intero_use_neomake = 0

augroup interoMaps
  au!

  au FileType haskell nnoremap <silent> <leader>io :InteroOpen<CR>
  au FileType haskell nnoremap <silent> <leader>iov :InteroOpen<CR><C-W>H
  au FileType haskell nnoremap <silent> <leader>ih :InteroHide<CR>
  au FileType haskell nnoremap <silent> <leader>is :InteroStart<CR>
  au FileType haskell nnoremap <silent> <leader>ik :InteroKill<CR>

  au FileType haskell nnoremap <silent> <leader>wr :w \| :InteroReload<CR>
  au FileType haskell nnoremap <silent> <leader>il :InteroLoadCurrentModule<CR>
  au FileType haskell nnoremap <silent> <leader>if :InteroLoadCurrentFile<CR>

  au FileType haskell map <leader>t <Plug>InteroGenericType
  au FileType haskell map <leader>T <Plug>InteroType
  au FileType haskell nnoremap <silent> <leader>it :InteroTypeInsert<CR>

  au FileType haskell nnoremap <silent> <leader>jd :InteroGoToDef<CR>
  au FileType haskell nnoremap <silent> <leader>iu :InteroUses<CR>
  au FileType haskell nnoremap <leader>ist :InteroSetTargets<SPACE>
augroup END
