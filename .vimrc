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
" set gdefault " substituion flag 'g' on by default
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
set laststatus=2 " show status line
" set relativenumber " show relative line numbers to current cursor position
set wrap " wrap text if longer than window width
set textwidth=79 " max text insertion width before breakage
set formatoptions=qrn1 " describition of automatic formatting
if exists('+colorcolumn')
  set colorcolumn=85 " highlight column
endif
set autowriteall " auto-save the file on different commands
set statusline=[%l,%v\ %P%M]\ %f\ %r%h%w\ (%{&ff}) " Useful status information at bottom of screen
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

set background=light
let g:yankring_history_dir="~/.vim/tmp"
let g:yankring_history_file="yankie"

colorscheme solarized

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

" use Q for formatting the current paragraph (or selection)
vmap Q gq
nmap Q gqap

" force home row usage
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>

" step line by line even when text is wrapped
nnoremap j gj
nnoremap k gk

" easy window navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

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
map <F1> <ESC>

" reselect just pasted text
nnoremap <leader>v V`]

" escape from insert mode
inoremap jj <ESC>

" rewrap hard a paragraph
nnoremap <leader>q gqip

nmap <leader><W> :%s/\s\+$\| \+\ze\t//g<cr>
" nnoremap <leader>W :%s/\s\+$//<cr>:let @/=''<CR>
" nnoremap <leader>W :%s/\s\+$\| \+\ze\t//<cr>:let @/=''<CR>
nnoremap <leader>w <C-w>v<C-w>l

"" Bash out extra whitespace
highlight ExtraWhitespace ctermfg=15 ctermbg=4 guifg=#CF6A4C guibg=#420E09
match ExtraWhitespace /\s\+$\| \+\ze\t/

au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif " remember cursor position


highlight CursorLine guibg=Gray20
"" filetype matches
au! BufRead,BufNewFile *.json setfiletype json

"" CoffeeScript
" let coffee_compile_on_save = 1 " auto compile on write/save
let coffee_no_trailing_space_error = 1
let coffee_no_trailing_semicolon_error = 1
let coffee_no_reserved_words_error = 1

if has("gui_macvim")
  colorscheme wombat
  set antialias               " MacVim: smooth fonts.
  set sidescroll=5 " side scrolling from :h wrap
  set listchars+=precedes:<,extends:>
  set listchars=tab:▸\ ,trail:.,eol:¬ " Use the same symbols as TextMate for tabstops and EOL


  let macvim_skip_cmd_opt_movement = 1
  set fuoptions=maxvert,maxhorz
  " Fullscreen on start
  au GUIEnter * set fullscreen
  " hide toolbar
  " hide scrollbars
  set go-=T
  set go-=r
  set go-=R
  set go-=l
  set go-=L
  set mousefocus
  macmenu &File.New\ Tab key=<nop>

  "set undofile

  set lines=999

  " color of statusline changes according to mode
  function! InsertStatuslineColor(mode)
    if a:mode == 'i'
      highlight statusline guibg=#e5786d
    elseif a:mode == 'r'
      highlight statusline guibg=#8ac6f2
    else
      highlight statusline guibg=red
    endif
  endfunction

  au InsertEnter * call InsertStatuslineColor(v:insertmode)
  au InsertLeave * hi statusline guibg=#8ac6f2

  " default the statusline to green when entering Vim
  highlight statusline guifg=#444444 guibg=#8ac6f2 gui=none

  " nice max col mark
  highlight ColorColumn guibg=black

  " cursor colors
  highlight Cursor guifg=black guibg=#8ac6f2
  highlight iCursor guifg=red guibg=#e5786d

  au FocusLost * :wa
endif
