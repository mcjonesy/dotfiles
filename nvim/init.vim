""""Leader key
let mapleader = ','

" Specify a directory for plugins (for Neovim: ~/.local/share/nvim/plugged)
call plug#begin('~/.vim/plugged')

" Make sure you use single quotes

" For aligning lines of text, typically useful to make things line up by '='
" Set to take 'ga' as the command below
" Type gaip= to align around = in the current paragraph
Plug 'junegunn/vim-easy-align'

" For file/session/tab/buffer/workspace management, *and* fuzzy searching!!
Plug 'vim-ctrlspace/vim-ctrlspace'

" Commenting and uncommenting lines of code using ,cc and ,cu
Plug 'scrooloose/nerdcommenter'

" File explorer
Plug 'scrooloose/nerdtree'

" All sorts of things relating to surrounding text objects. Super useful with quotes
Plug 'tpope/vim-surround'

" Syntax highlighting, error linting, code completion and navigation. The
" whole shebang
Plug 'Valloric/YouCompleteMe'

" Git magic, mainly for Gblame and Ggrep
Plug 'tpope/vim-fugitive'

" Git line-by-line change notifications while editing files
" Use ]c and [c to jump between changes
Plug 'airblade/vim-gitgutter'

" Status line at the bottom of the screen. Needs more configuration to work
" best
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Switch between header and source
Plug 'derekwyatt/vim-fswitch'

" A better terminal experience
Plug 'vimlab/split-term.vim'

" Run commands as you type, REPL style! Works on C++ if you get Cling set up,
" which is easy
Plug 'metakirby5/codi.vim'

" Reflowing arguments and parameters on to multiple lines
Plug 'AndrewRadev/splitjoin.vim'

" Visualises vim's undo tree
Plug 'mbbill/undotree'

"Plug 'huawenyu/neogdb.vim'

"""" Things to do
" configure airline more
" configure gitgutter more
" add extra shortcuts for YCM code navigation and fixit tools
" figure out how workspaces really work in ctrlspace
" try UltiSnips

" Initialize plugin system
call plug#end()

""""""""My preferences

""""Handy function for opening files under the cursor in a different window
function! OpenUnderCursor()
    let file_name = expand('<cfile>') 
    if !strlen(file_name) 
        echo 'NO FILE UNDER CURSOR' 
        return 
    endif 

    " look for a line number separated by a : 
    if search('\%#\f*:\zs[0-9]\+') 
        " change the 'iskeyword' option temporarily to pick up just numbers 
        let temp = &iskeyword 
        set iskeyword=48-57 
        let line_number = expand('<cword>') 
        exe 'set iskeyword=' . temp 
    endif 

    wincmd w
    exe 'e '.file_name 
    if exists('line_number') 
        exe line_number 
    endif 
endfunction

""""Appearance/Colours
set background=dark

""""Tabs
"How big to show existing tabs
set tabstop=4
"How far to shift when using the > command
set shiftwidth=4
"How to interpret the tab key
set expandtab

""""Line numbering
set number
set norelativenumber

""""Remove highlighting
nmap \ :nohlsearch<CR>

""""Misc
"Required by vim-ctrlspace
set nocompatible
set hidden
set showtabline=0

""""Convenient keymappings
nmap <F7> :20Term ./configure.py --oclint -b Release -c -p 
cmap <F7> embedded_estimator<CR>
nmap <F8> :w<CR> :20Term ./build-tools/Fedora/build.py<CR>
tmap <F8> <Esc><C-w><C-w>

nmap <C-h> <C-w>h
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-l> <C-w>l

nmap <F2> : call OpenUnderCursor()<CR>

nmap  <buffer> <silent> k gk
nmap  <buffer> <silent> j gj
nmap  <buffer> <silent> 0 g0
nmap  <buffer> <silent> $ g$

""""I never use s anyway
nmap s :w<CR>

""""Persist undos between nvim sessions
set undofile

"""""""" Easy align settings
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

"""""""" Vim-Ctrlspace settings
"if executable("ag")
    "let g:CtrlSpaceGlobCommand = 'ag -l --nocolor -g ""'
"endif
let g:CtrlSpaceSearchTiming = 1

let g:CtrlSpaceDefaultMappingKey = "<C-p>"

let g:CtrlSpaceLoadLastWorkspaceOnStart = 1
let g:CtrlSpaceSaveWorkspaceOnSwitch = 1
let g:CtrlSpaceSaveWorkspaceOnExit = 1

hi link CtrlSpaceSearch IncSearch
hi link CtrlSpaceSelected StatusLine

"""""""" NerdCommenter settings
let g:NERDTrimTrailingWhiteSpace = 1

"""""""" YouCompleteMe
highlight YcmErrorSign      ctermbg=9
highlight YcmErrorLine      ctermbg=52
highlight YcmErrorSection   ctermbg=52
highlight YcmWarningSign    ctermbg=17
highlight YcmWarningLine    ctermbg=17
highlight YcmWarningSection ctermbg=17

let g:ycm_show_diagnostics_ui = 1
let g:ycm_enable_diagnostic_highlighting = 1

nmap <F12> :YcmCompleter GoTo<CR>
nmap <F5>  :YcmForceCompileAndDiagnostics<CR>
nmap <F3>  :YcmCompleter GetParent<CR>
nmap <F4>  :YcmCompleter FixIt<CR>

autocmd Filetype c,cpp,hpp nmap <CR>  :YcmCompleter GetType<CR>

let g:ycm_always_populate_location_list = 1

"""""""" Airline
let g:airline_powerline_fonts = 1
let g:airline#extensions#ycm#error_symbol = 'E:'
function! AirlineInit()
    let g:airline_section_error = airline#section#create(['ycm_error_count'])
endfunction
autocmd VimEnter * call AirlineInit()

"""""""" FSwitch
nmap <silent> <Leader>ff :FSHere<cr>
nmap <silent> <Leader>fl :FSSplitRight<cr>
nmap <silent> <Leader>fh :FSSplitLeft<cr>

"""""""" split term
set splitbelow

"""""""" undotree
nmap <F6> :UndotreeToggle<CR>

"""""""" pretty print xml
function! DoPrettyXML()
  " save the filetype so we can restore it later
  let l:origft = &ft
  set ft=
  " delete the xml header if it exists. This will
  " permit us to surround the document with fake tags
  " without creating invalid xml.
  1s/<?xml .*?>//e
  " insert fake tags around the entire document.
  " This will permit us to pretty-format excerpts of
  " XML that may contain multiple top-level elements.
  0put ='<PrettyXML>'
  $put ='</PrettyXML>'
  silent %!xmllint --format -
  " xmllint will insert an <?xml?> header. it's easy enough to delete
  " if you don't want it.
  " delete the fake tags
  2d
  $d
  " restore the 'normal' indentation, which is one extra level
  " too deep due to the extra tags we wrapped around the document.
  silent %<
  " back to home
  1
  " restore the filetype
  exe "set ft=" . l:origft
endfunction
command! PrettyXML call DoPrettyXML()

"""""""" Codi
 let g:codi#interpreters = {
       \ 'python3': {
           \ 'bin': 'python3',
           \ 'prompt': '^\(>>>\|\.\.\.\) ',
           \ },
       \ }
