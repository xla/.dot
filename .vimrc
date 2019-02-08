" For a paranoia.
" Normally `:set nocp` is not needed, because it is done automatically
" when .vimrc is found.
if &compatible
  " `:set nocp` has many side effects. Therefore this should be done
  " only when 'compatible' is set.
  set nocompatible
endif

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
set novisualbell
" don't beep
set noerrorbells
" prevent auto indentation when pasting
set pastetoggle=<F2>
" font family & size
set guifont=PragmataProLiga-Regular:h18
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
" avoid excessive redraws
set lazyredraw
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
" always show signcolumns
set signcolumn=yes
" better display of messages
set cmdheight=2
" smaller updatetime for CursorHold & CursorHoldI
set updatetime=300
" split at the bottom
set splitbelow

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

" set completeopt-=longest
" set completeopt-=menu
" set completeopt-=preview

" Set python paths explicitly under macOS.
if has('macunix')
  let g:python2_host_prog = '/usr/local/bin/python'
  let g:python3_host_prog = '/usr/local/bin/python3'
endif

" switch syntax highlighting on, when the terminal has colors
if &t_Co > 2
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
au BufNewFile,BufReadPost      *.md        set filetype=markdown

" plugin management with minpac
function! PackInit() abort
  packadd minpac

  if exists('*minpac#init')
    call minpac#init()
    call minpac#add('k-takata/minpac', {'type': 'opt'})

    " colorscheme
    call minpac#add('cloudhead/shady.vim')

    " comments
    call minpac#add('tpope/vim-commentary')

    " git
    call minpac#add('mhinz/vim-signify')

    " navigation
    call minpac#add('cloudhead/neovim-fuzzy')
    call minpac#add('jremmen/vim-ripgrep')

    " intellisense
    call minpac#add('Shougo/denite.nvim')
    call minpac#add('neoclide/coc.nvim', {'branch': 'master', 'do': 'call coc#util#install()'})

    " markdown
    call minpac#add('plasticboy/vim-markdown')
    call minpac#add('junegunn/goyo.vim')
    call minpac#add('junegunn/limelight.vim')

    " purescript
    call minpac#add('purescript-contrib/purescript-vim')

    " es/jsx
    " call minpac#add('hail2u/vim-css3-syntax')
    " call minpac#add('pangloss/vim-javascript')
    " call minpac#add('MaxMEllon/vim-jsx-pretty')
    call minpac#add('neoclide/vim-jsx-improve', {'for': ['javascript', 'js']})
    call minpac#add('styled-components/vim-styled-components')

    " rst
    call minpac#add('gu-fan/riv.vim')

    " toml
    call minpac#add('cespare/vim-toml')
  endif
endfunction

command! PackClean call PackInit() | call minpac#clean()
command! PackStatus call PackInit() | call minpac#status()
command! PackUpdate call PackInit() | call minpac#update('', {'do': 'call minpac#status()'})

packloadall

" colors
try
  colorscheme shady
catch
endtry

" comments
nmap <C-_> <Plug>CommentaryLine
xmap <C-_> <Plug>Commentary

" fuzzy
nnoremap <leader>o :FuzzyOpen<cr>
nnoremap <leader>f :FuzzyGrep<cr>

" ripgrep
if executable('rg')
  let g:ackprg = 'rg --vimgrep --no-heading'
  set grepprg=rg\ --vimgrep
endif

" intellisense
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

inoremap <silent><expr> <TAB>
  \ pumvisible() ? "\<C-n>" :
  \ <SID>check_back_space() ? "\<TAB>" :
  \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
" use <c-space>for trigger completion
imap <c-space> coc#refresh()
" Use <cr> for confirm completion.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" use <c-space>for trigger completion
imap <c-space> coc#refresh()

autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif

" Remap keys for gotos
nmap <silent> gd :call CocAction('jumpDefinition')<cr>
nmap <silent> gj :call CocAction('jumpTypeDefinition')<cr>
nmap <silent> gi :call CocAction('jumpImplementation')<cr>
nmap <silent> gr :call CocAction('jumpReferences')<cr>

" Use K for show documentation in preview window
nnoremap <silent> K :call <sid>show_documentation()<cr>

function! s:show_documentation()
  if &filetype == 'vim'
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

command! -nargs=0 Prettier :call CocAction('runCommand', 'prettier.formatFile')
" Show diagnostics of current workspace
nnoremap <silent> <space>a  :<C-u>Denite coc-diagnostic<cr>

" markdown editing like it's iA Writer
let g:goyo_height = '90%'
let g:goyo_width = 120

" sane defaults to not error for inability to calculate dimming.
let g:limelight_conceal_ctermfg = 'gray'
let g:limelight_conceal_ctermfg = 240
let g:limelight_conceal_guifg = 'DarkGray'
let g:limelight_conceal_guifg = '#777777'
" Default: 0.5
let g:limelight_default_coefficient = 0.7
" Number of preceding/following paragraphs to include (default: 0)
let g:limelight_paragraph_span = 1
" Highlighting priority (default: 10)
"   Set it to -1 not to overrule hlsearch
let g:limelight_priority = -1

autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!

nnoremap <silent> <leader>z :Goyo<cr>

" rst
let g:riv_auto_format_table = 0
let g:riv_fold_auto_update = 0

" TODO list
command! Todo Rg 'TODO'
command! TodoLocal Rg 'TODO' %

nnoremap <leader>tg :Todo<cr>
nnoremap <leader>tl :TodoLocal<cr>

" custom highlight
hi User1 ctermbg=black ctermfg=red guibg=black guifg=red
