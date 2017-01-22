set number
set showcmd
set ruler 
:inoremap ( ()<Esc>i
:inoremap { {}<Esc>i
set nocompatible              
filetype off 
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Bundle "tpope/vim-pathogen"
Bundle 'sophacles/vim-processing'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-javascript'
Plugin 'Valloric/YouCompleteMe'
Plugin 'Townk/vim-autoclose'
call vundle#end()            
filetype plugin indent on
syntax on

execute pathogen#infect()

syntax enable
set background=dark
colorscheme solarized
