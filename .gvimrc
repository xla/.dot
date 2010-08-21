set nocompatible

filetype on
filetype plugin indent on         " Turn on file type detection.

syntax enable
syntax sync fromstart

set guifont=Pragmata:h15
" set guifont=Bitstream\ Vera\ Sans\ Mono:h13

"" indentation
set ts=2 sw=2 sts=2 et

set showcmd                       " Display incomplete commands.
set showmode                      " Display the mode you're in.

set autowriteall
set noai nocindent
set incsearch                     " Highlight matches as you type.
set hlsearch
set laststatus=2
set writebackup
set backup backupdir=$HOME/.vim/backup
set directory=/tmp
set ttyfast
set number
set ruler
set backspace=2     " allow backspacing over everything in insert mode

set title                         " Set the terminal's title

" Useful status information at bottom of screen
set statusline=[%l,%v\ %P%M]\ %f\ %r%h%w\ (%{&ff}) 

"" Suffixes that get lower priority when doing tab completion for filenames.
"" These are files we are not likely to want to edit or read.
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc,.pyc,.class,.jar

set fileformats=unix,dos,mac

set viminfo=!,'50,\"1000,:150,n~/.vim/viminfo
set fileformat=unix history=50
" set digraph

set title                         " Set the terminal's title

set visualbell                    " No beeping.
set ignorecase                    " Case-insensitive searching.
set smartcase                     " But case-sensitive if expression contains a capital letter.

" remember cursor position
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

"" Bash out extra whitespace
highlight CursorLine guibg=Gray20
highlight ExtraWhitespace ctermfg=15 ctermbg=4 guifg=#CF6A4C guibg=#420E09
match ExtraWhitespace /\s\+$\| \+\ze\t/
 
" change working directory automatically
" set autochdir

" set lines=55
" set columns=100
 
":au BufWinEnter * let w:m2=matchadd('ErrorMsg', '\%>80v.\+', -1)

:colorscheme vividchalk

if has("gui_macvim")
  set guifont=Inconsolata:h18 " Font family and font size.
  set antialias               " MacVim: smooth fonts.
  set encoding=utf-8          " Use UTF-8 everywhere.

  " side scrolling from :h wrap
  set sidescroll=5
  set listchars+=precedes:<,extends:>

  " Shortcut to rapidly toggle `set list`
  nmap <leader>l :set list!<CR>


  " Use the same symbols as TextMate for tabstops and EOL
  set listchars=tab:▸\ ,eol:¬

  let macvim_skip_cmd_opt_movement = 1
  set fuoptions=maxvert,maxhorz
  au GUIEnter * set fullscreen

  " hide toolbar
  set go-=T

  " hide scrollbars
  set go-=r
  set go-=R
  set go-=l
  set go-=L
  set mousefocus

  macmenu &File.New\ Tab key=<nop>
  map <D-t> <Plug>PeepOpen
endif
