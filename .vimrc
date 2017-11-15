set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'bitc/vim-hdevtools'
Plugin 'mhinz/vim-startify'
Plugin 'tpope/vim-sleuth'
Plugin 'Raimondi/delimitMate'
Plugin 'tpope/vim-fugitive'
Plugin 'vim-syntastic/syntastic'
Plugin 'ElmCast/elm-vim'
Plugin 'scrooloose/nerdtree'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
call vundle#end() 
filetype plugin indent on
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1

let g:elm_syntastic_show_warnings = 1
let g:syntastic_elm_checkers = ['elm_make']
let g:airline_powerline_fonts = 1
nmap <leader>sp :call <SID>SynStack()<CR>
function! <SID>SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

set nowrap
set ruler
set number

set expandtab
set shiftwidth=4
set tabstop=4
set autoindent 

set hlsearch
set incsearch
set ignorecase
set smartcase

set encoding=utf-8

set splitbelow
set splitright

set noswapfile

set ai
set si

syntax on
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K> 
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
