" pathogen bootstrap
filetype off
call pathogen#runtime_append_all_bundles()
filetype plugin indent on

set nocompatible
set modelines=0

syntax enable
syntax sync fromstart

colorscheme wombat
set guifont=Inconsolata:h18         " Font family and font size.
set ts=2 sw=2 sts=2 et

set encoding=utf-8                  " Use UTF-8 everywhere.
set scrolloff=3
set autoindent
set showmode                        " Display the mode you're in.
set showcmd                         " Display incomplete commands.
set hidden
set wildmenu
set wildmode=list:longest
set visualbell                      " No beeping.
set cursorline
set ttyfast
set ruler
set backspace=indent,eol,start      " allow backspacing over everything in insert mode
set laststatus=2

nnoremap / /\v
vnoremap / /\v
set ignorecase                      " Case-insensitive searching.
set smartcase                       " But case-sensitive if expression contains a capital letter.
set gdefault
set incsearch                       " Highlight matches as you type.
set showmatch
set hlsearch
nnoremap <leader><space> :noh<cr>

set wrap
set textwidth=79
set formatoptions=qrn1

nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
vnoremap <up> <nop>
vnoremap <down> <nop>
vnoremap <left> <nop>
vnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
" inoremap <left> <nop>
" inoremap <right> <nop>
nnoremap j gj
nnoremap k gk

inoremap <F1> <ESC>
nnoremap <F1> <ESC>
vnoremap <F1> <ESC>

nnoremap ; :

nnoremap <leader>W :%s/\s\+$\| \+\ze\t//<cr>:let @/=''<CR>
nnoremap <leader>a :Ack
nnoremap <leader>ft Vatzf
nnoremap <leader>v V`]
nnoremap <leader>ev <C-w><C-v><C-l>:e $MYVIMRC<cr>

inoremap jj <ESC>

nnoremap <leader>w <C-w>v<C-w>l
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

set grepprg=ack\ -a

set autowriteall
set noai nocindent
set writebackup
set backup backupdir=$HOME/.vim/backup
set directory=/tmp
set number
set title                         " Set the terminal's title
set statusline=[%l,%v\ %P%M]\ %f\ %r%h%w\ (%{&ff}) " Useful status information at bottom of screen
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc,.pyc,.class,.jar " Suffixes that get lower priority when doing tab completion for filenames.
set fileformats=unix,dos,mac
set viminfo=!,'50,\"1000,:150,n~/.vim/viminfo
set fileformat=unix history=50
" set digraph
" set lines=55
" set columns=100
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif " remember cursor position
"" Bash out extra whitespace
highlight CursorLine guibg=Gray20
highlight ExtraWhitespace ctermfg=15 ctermbg=4 guifg=#CF6A4C guibg=#420E09
match ExtraWhitespace /\s\+$\| \+\ze\t/

nmap <leader>l :set list!<CR> " Shortcut to rapidly toggle `set list`

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

  set colorcolumn=85

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

  set relativenumber
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
