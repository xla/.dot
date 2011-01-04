set nocompatible

filetype on
filetype plugin indent on " Turn on file type detection.
syntax enable
syntax sync fromstart

colorscheme wombat
set guifont=Inconsolata:h18 " Font family and font size.
set ts=2 sw=2 sts=2 et
set backspace=2     " allow backspacing over everything in insert mode
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
set title                         " Set the terminal's title
set statusline=[%l,%v\ %P%M]\ %f\ %r%h%w\ (%{&ff}) " Useful status information at bottom of screen
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc,.pyc,.class,.jar " Suffixes that get lower priority when doing tab completion for filenames.
set fileformats=unix,dos,mac
set viminfo=!,'50,\"1000,:150,n~/.vim/viminfo
set fileformat=unix history=50
set visualbell                    " No beeping.
set ignorecase                    " Case-insensitive searching.
set smartcase                     " But case-sensitive if expression contains a capital letter.
" set digraph
" set lines=55
" set columns=100
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif " remember cursor position
"" Bash out extra whitespace
highlight CursorLine guibg=Gray20
highlight ExtraWhitespace ctermfg=15 ctermbg=4 guifg=#CF6A4C guibg=#420E09
match ExtraWhitespace /\s\+$\| \+\ze\t/

nmap <leader>l :set list!<CR> " Shortcut to rapidly toggle `set list`

if has("gui_macvim")
  colorscheme wombat
  set antialias               " MacVim: smooth fonts.
  set encoding=utf-8          " Use UTF-8 everywhere.
  set sidescroll=5 " side scrolling from :h wrap
  set listchars+=precedes:<,extends:>
  set listchars=tab:▸\ ,trail:.,eol:¬ " Use the same symbols as TextMate for tabstops and EOL

  let macvim_skip_cmd_opt_movement = 1
  set fuoptions=maxvert,maxhorz
  " Fullscreen on start
  " au GUIEnter * set fullscreen
  " hide toolbar
  set go-=T
  " hide scrollbars
  set go-=r
  set go-=R
  set go-=l
  set go-=L
  set mousefocus
  macmenu &File.New\ Tab key=<nop>

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
endif
