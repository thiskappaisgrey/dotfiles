call plug#begin('~/.vim/plugged')
Plug 'scrooloose/nerdtree'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'Raimondi/delimitMate'
Plug 'mhinz/vim-startify'
Plug 'rakr/vim-one'
Plug 'whatyouhide/vim-gotham'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'carlitux/deoplete-ternjs'
Plug 'steelsojka/deoplete-flow'
Plug 'Shougo/neosnippet'
Plug 'Shougo/neosnippet-snippets'
Plug 'w0rp/ale'
Plug 'othree/yajs.vim'
Plug 'othree/es.next.syntax.vim'
Plug 'alvan/vim-closetag'
Plug 'mxw/vim-jsx'
Plug 'hail2u/vim-css3-syntax'
Plug 'hzchirs/vim-material'
Plug 'ap/vim-css-color'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'ryanoasis/vim-devicons'
call plug#end()
let g:airline_powerline_fonts = 1
let g:deoplete#enable_at_startup = 1
function! StrTrim(txt)
  return substitute(a:txt, '^\n*\s*\(.\{-}\)\n*\s*$', '\1', '')
endfunction

let g:flow_path = StrTrim(system('PATH=$(npm bin):$PATH && which flow'))

if g:flow_path != 'flow not found'
  let g:deoplete#sources#flow#flow_bin = g:flow_path
endif
let g:neosnippet#enable_completed_snippet = 1
let g:airline_theme='material'
let g:airline#extensions#tabline#enabled = 1
let g:javascript_plugin_jsdoc = 1
let g:javascript_plugin_flow = 1
let g:ale_linters = {
\   'javascript': ['eslint', 'flow']
\}
let g:ale_fixers = {
\     'javascript': ['eslint', 'prettier'],
\     'css': ['prettier']
\}
let g:ale_fix_on_save = 1
let g:ale_javascript_prettier_use_local_config = 1
let g:jsx_ext_required = 0
let g:closetag_filenames = "*.html, *.js"
let mapleader = '\'

set nowrap
set number
set expandtab
set shiftwidth=2
set tabstop=2

set ignorecase
set smartcase

set encoding=utf-8

set splitbelow
set splitright

set noswapfile

set ai
set si

set autoread

set backspace=2
set nofoldenable

set termguicolors
colorscheme vim-material 

syntax on
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
tnoremap <Esc> <C-\><C-n>
nnoremap <silent> <C-l> :nohl<CR><C-l>

" deoplete tab-complete
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"
nmap <leader>d <Plug>(ale_fix)
" copy paste
vnoremap  <leader>y  "+y
nnoremap  <leader>Y  "+yg_
nnoremap  <leader>y  "+y
nnoremap  <leader>yy  "+yy

nnoremap <leader>p "+p
nnoremap <leader>P "+P
vnoremap <leader>p "+p
vnoremap <leader>P "+P
