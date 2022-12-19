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
set cmdheight=3
" smaller updatetime for CursorHold & CursorHoldI
set updatetime=300
" split at the bottom
set splitbelow
" some file watching tools will miss an rename and replace
set backupcopy=yes

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
map <C-n> :lnext<cr>
map <C-m> :lprevious<cr>
nnoremap <leader>a :LToggle<cr>

" map make
nmap <leader>m :make!<cr>

" remember cursor position
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

" Per file-type indentation
au FileType                     haskell         setlocal sts=4 sw=4 expandtab
au FileType                     elm             setlocal sts=4 sw=4 expandtab
au FileType                     javascript      setlocal fo=cqt sts=2 sw=2 tw=80 wm=0 expandtab
au FileType                     css             setlocal ts=2  sw=2 noexpandtab
au FileType                     go              setlocal ts=4  sw=4 noexpandtab
au BufNewFile,BufRead,FileType  *.go            setlocal ts=4  sw=4 noexpandtab
au FileType                     c,cpp,glsl      setlocal ts=8  sw=8 noexpandtab
au FileType                     lua             setlocal       sw=4 expandtab
au FileType                     sh,zsh          setlocal ts=2  sw=2 noexpandtab
au FileType                     vim,ruby        setlocal sts=2 sw=2 expandtab
au FileType                     help            setlocal ts=4  sw=4 noexpandtab
au FileType                     txt             setlocal noai nocin nosi inde= wrap linebreak
au FileType                     pandoc          setlocal nonumber
au FileType                     markdown        setlocal nonumber
au FileType                     fountain        setlocal nonumber noai nocin nosi inde= wrap linebreak
au BufNewFile,BufReadPost       *.md            set filetype=markdown
au BufNewFile,BufRead           *.tsx,*.jsx     set filetype=typescriptreact

augroup configgroup
    autocmd!

    "Set Pollen syntax for files with these extensions:
    au! BufRead,BufNewFile *.p set filetype=pollen
    au! BufRead,BufNewFile *.pm set filetype=pollen
    au! BufRead,BufNewFile *.pp set filetype=pollen
    au! BufRead,BufNewFile *.ptree set filetype=pollen

    " Suggested editor settings:
    autocmd FileType pollen setlocal wrap      " Soft wrap (don't affect buffer)
    autocmd FileType pollen setlocal linebreak " Wrap on word-breaks only
augroup END

" rust.vim sets the filetype for Cargo.toml to cfg, which confuses vim-toml
au BufNewFile,BufRead *.toml,Gopkg.lock,Cargo.lock,*/.cargo/config,*/.cargo/credentials,Pipfile setf toml

" plugin management with minpac
function! PackInit() abort
  packadd minpac

  if exists('*minpac#init')
    call minpac#init()
    call minpac#add('k-takata/minpac', {'type': 'opt'})

    " comments
    call minpac#add('tpope/vim-commentary')

    " git
    " call minpac#add('mhinz/vim-signify')

    " navigation
    call minpac#add('cloudhead/neovim-fuzzy')
    call minpac#add('jremmen/vim-ripgrep')

    " intellisense
    call minpac#add('Shougo/denite.nvim')
    call minpac#add('neoclide/coc.nvim', {'branch': 'master', 'do': 'call coc#util#install()'})

    " elm
    call minpac#add('w0rp/ale')
    call minpac#add('elmcast/elm-vim')

    " toml
    call minpac#add('cespare/vim-toml')

    " glsl
    call minpac#add('tikhomirov/vim-glsl')

    " go
    call minpac#add('fatih/vim-go')

    " graphql
    call minpac#add('jparise/vim-graphql')

    " latex
    call minpac#add('lervag/vimtex')

    " pico8
    " call minpac#add('justinj/vim-pico8-syntax')

    " purescript
    " call minpac#add('purescript-contrib/purescript-vim')
    " call minpac#add('FrigoEU/psc-ide-vim')
    " call minpac#add('vim-syntastic/syntastic')

    " ocaml/reason
    call minpac#add('reasonml-editor/vim-reason-plus')
    call minpac#add('sbdchd/neoformat')

    " one
    call minpac#add('rakr/vim-one')

    " racket
    call minpac#add('otherjoel/vim-pollen')
    call minpac#add('wlangstroth/vim-racket')

    " rst
    call minpac#add('gu-fan/riv.vim')

    " rust
    call minpac#add('rust-lang/rust.vim')

    " shady
    call minpac#add('cloudhead/shady.vim')

    " solidity
    call minpac#add('tomlion/vim-solidity')

    " svelte
    call minpac#add('leafgarland/typescript-vim')
    call minpac#add('evanleck/vim-svelte')

    " terraform
    call minpac#add('hashivim/vim-terraform')
    " call minpac#add('juliosueiras/vim-terraform-completion')

    " typescript
    call minpac#add('peitalin/vim-jsx-typescript')

    " writer
    call minpac#add('junegunn/goyo.vim')
    call minpac#add('junegunn/limelight.vim')
    call minpac#add('reedes/vim-colors-pencil')
    call minpac#add('subnut/vim-iawriter')
    call minpac#add('preservim/vim-pencil')
  endif
endfunction

command! PackClean call PackInit() | call minpac#clean()
command! PackStatus call PackInit() | call minpac#status()
command! PackUpdate call PackInit() | call minpac#update('', {'do': 'call minpac#status()'})

" Plugins need to be added to runtimepath before helptags can be generated.
packloadall
" Load all of the helptags now, after plugins have been loaded.
" Ignore all messages and errors.
silent! helptags ALL

" colors
try
  colorscheme shady
  set background=dark
catch
endtry

" comments
nmap <C-_> <Plug>CommentaryLine
xmap <C-_> <Plug>Commentary

" fuzzy
nnoremap <leader>o :FuzzyOpen<cr>
nnoremap <leader>f :FuzzyGrep<cr>

" vim-commentary
nmap <C-_> gcc
xmap <C-_> gc

" ripgrep
if executable('rg')
  let g:ackprg = 'rg --vimgrep --no-heading'
  set grepprg=rg\ --vimgrep
endif

" ListToggle
let g:lt_location_list_toggle_map = '<leader>a'
let g:lt_height = 5

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

" shortcuts for rakcet & pollen
imap <C-L> λ
imap <C-E> ◊

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

" TODO list
command! Todo Rg 'TODO'
command! TodoLocal Rg 'TODO' %

nnoremap <leader>tg :Todo<cr>
nnoremap <leader>tl :TodoLocal<cr>

" elm
let g:elm_jump_to_error = 1
let g:elm_make_show_warnings = 1
let g:elm_detailed_complete = 1
let g:elm_format_autosave = 1
let g:elm_format_fail_silently = 0
let g:elm_setup_keybindings = 1

" go
let g:go_fmt_command = "goimports"
let g:go_gopls_enabled = 1

" ocaml/reason
let g:neoformat_enabled_ocaml = ['ocamlformat']

" purs
let g:psc_ide_syntastic_mode = 1
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1

" rst
let g:riv_auto_format_table = 0
let g:riv_fold_auto_update = 0

" rust
let g:rustfmt_autosave = 1

" terraform
let g:terraform_align=1
let g:terraform_fmt_on_save=1

" vimtex
let g:vimtex_view_general_viewer = 'mupdf'
let g:vimtex_view_general_options
    \ = '-reuse-instance -forward-search @tex @line @pdf'
" let g:vimtex_view_general_options_latexmk = '-reuse-instance'
let g:tex_flavor  = 'xelatex'
let g:tex_conceal = ''
let g:vimtex_fold_manual = 1
let g:vimtex_compiler_latexmk = {
        \ 'executable' : 'latexmk',
        \ 'options' : [
        \   '-xelatex',
        \   '-file-line-error',
        \   '-synctex=1',
        \   '-interaction=nonstopmode',
        \ ],
        \}

nnoremap <leader>c :VimtexCompile<cr>

" writer
let g:pencil_higher_contrast_ui = 1

" custom highlight
" hi User1 ctermbg=black ctermfg=red guibg=black guifg=red
hi CocErrorSign ctermfg=red guibg=black guifg=red
hi CocWarningSign ctermfg=yellow guibg=black guifg=yello

" CoC
hi link CocErrorVirtualText    Error
hi link CocErrorSign           Error
hi      CocErrorHighlight      cterm=undercurl guisp=#B03060
hi link CocErrorFloat          Error

hi link CocWarningVirtualText  Warning
hi link CocWarningSign         Warning
hi      CocWarningHighlight    cterm=undercurl guisp=#FFE4B5

hi link CocInfoVirtualText     Alternative
hi link CocInfoSign            Alternative
hi      CocInfoHighlight       cterm=underline guisp=blue

hi link CocHintVirtualText     Alternative
hi link CocHintSign            Alternative
hi      CocHintHighlight       cterm=none guisp=blue
hi      CocUnusedHighlight     ctermfg=246 cterm=strikethrough

hi link CocRustChainingHint    Hint
" These only use an 'undercurl'. The colors used are
" 'maroon' and 'moccasin'.
hi link CocHintSign            Hint
hi link CocCodeLens            DarkGrey
hi link CocFloating            Pmenu

" helper functions
command! LToggle call s:LListToggle()

function! s:LListToggle() abort
    let buffer_count_before = s:BufferCount()
    " Location list can't be closed if there's cursor in it, so we need
    " to call lclose twice to move cursor to the main pane
    silent! lclose
    silent! lclose

    if s:BufferCount() == buffer_count_before
        execute "silent! lopen 10"
    endif
endfunction

function! s:BufferCount() abort
    return len(filter(range(1, bufnr('$')), 'buflisted(v:val)'))
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()
nmap <c-a> <Plug>(coc-codeaction)
