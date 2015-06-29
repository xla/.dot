" must be first to change other options as side effect
set nocompatible

set shell=/bin/sh

filetype off
" pathogen bootstrap
call pathogen#helptags()
call pathogen#runtime_append_all_bundles()

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

set background=light
colorscheme solarized

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

" ctrlp
map <leader>t :CtrlP<cr>
let g:ctrlp_working_path_mode=2

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

nmap <leader>b :GoBuild<cr>
nmap <leader>m :GoTest<cr>

" remap help file
inoremap <F1> <ESC>
" escape from insert mode
inoremap jk <ESC>

" remember cursor position
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

" linting & vetting for go files
autocmd FileType go autocmd BufWritePost <buffer> GoVet
autocmd FileType go autocmd BufWritePost <buffer> GoLint

let g:go_fmt_command = "goimports"
let g:go_auto_type_info = 1
let g:go_fmt_experimental = 1
