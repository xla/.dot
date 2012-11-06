set shell=/bin/sh

set nocompatible " must be first to change other options as side effect

filetype off
" pathogen bootstrap
call pathogen#helptags()
call pathogen#runtime_append_all_bundles()

" force reload of ftdetect files
filetype plugin indent on

set hidden " hide buffers instead of closing them
set expandtab " use correct amount of spaces for a tab in insert mode
set tabstop=2 " a tab is two spaces
set softtabstop=2
set shiftwidth=2 " number of spaces to use for autoindenting
set shiftround    " use multiple of shiftwidth when indenting with '<' and '>'
set modelines=0 " avoid modeline vulnerabilities
set backspace=indent,eol,start " allow backspacing over everything in insert mode
set smarttab      " insert tabs on the start of a line according to shiftwidth, not tabstop
set autoindent " copies indentation from previous line
set copyindent " copy the previous indentation on autoindenting
set number " always show line numbers
set showmatch   " set show matching parenthesis
set ignorecase " ignore case when searching
set smartcase  " ignore case if search pattern is all lowercase, case-sensitive otherwise
set hlsearch   " highlight search terms
set incsearch  " show search matches as you type
set history=1000         " remember more commands and search history
set undolevels=1000      " use many muchos levels of undo
set wildignore+=*.swp,*.bak,*.pyc,*.class,.git,node_modules/**
set title                " change the terminal's title
set visualbell           " don't beep
set noerrorbells         " don't beep
set pastetoggle=<F2> " prevent auto indentation when pasting
set guifont=Inconsolata:h18         " font family & size
set encoding=utf-8                  " use utf-8 everywhere
set scrolloff=3 " minimal lines to kepp above and below screen
set showmode                        " display the mode you're in.
set showcmd                         " display incomplete commands.
set wildmenu " enhanced completion
set wildmode=list:longest " enhanced completion
set cursorline " highlight the line of the cursor
set ttyfast " smooth and fast redrawing
set ruler " show line and column info
set wrap " wrap text if longer than window width
set textwidth=79 " max text insertion width before breakage
set formatoptions=qrn1 " describition of automatic formatting
set autowriteall " auto-save the file on different commands
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc,.pyc,.class,.jar " Suffixes that get lower priority when doing tab completion for filenames.
set fileformat=unix " EOL for current buffer
set fileformats=unix,dos,mac " list of EOL formats to try
set viminfo=!,'50,\"1000,:150,n~/.vim/viminfo " store history information

"" backup/swap/undo
set backup " enable backups
set backupdir=~/.vim/tmp/backup/ " backup file directory
set directory=~/.vim/tmp/swap/ " swap file directory
if exists('+undodir')
  set undodir=~/.vim/tmp/undo/ " undo file directory
endif
set writebackup " backup before overwritting

set background=dark

colorscheme kyle

hi User1 ctermfg=94 ctermbg=0

set laststatus=2          " show status line
set statusline=
set statusline +=\ %1*%m  "modified flag
set statusline +=%*%t
set statusline +=\ [%2l/%L] "current line

if &t_Co > 2 || has("gui_running")
  " switch syntax highlighting on, when the terminal has colors
  syntax enable
  syntax sync fromstart
endif

let mapleader="," " change the mapleader from \ to ,

" spare two strokes for command invocation
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
" remap help file
inoremap <F1> <ESC>
" escape from insert mode
inoremap jk <ESC>

au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif " remember cursor position

highlight CursorLine guibg=Gray20
set makeprg=go\ test\ ./...
