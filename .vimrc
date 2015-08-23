if has('vim_starting')
  set nocompatible                                         " Be iMproved
endif

if has('nvim')
  let is_nvim=1
endif

"-------------------------------------------------------------------------------
" Setup leader key
"-------------------------------------------------------------------------------
nnoremap <SPACE> <Nop>
let mapleader = " "

"-------------------------------------------------------------------------------
" Bundle definitions
"-------------------------------------------------------------------------------
call plug#begin('~/.vim/plugged')

Plug 'vim-scripts/listmaps.vim'
Plug 'bling/vim-airline'
let g:airline_powerline_fonts = 1

Plug 'Odie/Smart-Tabs'

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes \| ./install' }
nnoremap <silent> <C-P> :FZF<CR>

" Plug 'kien/ctrlp.vim'
" let g:ctrlp_follow_symlinks = 2   " Always follow symlinks
" let g:ctrlp_user_command = {
" 	\ 'types': {
"  	\     1: ['.git', 'cd %s && git ls-files --cached --exclude-standard --others'],
"  	\     2: ['.hg', 'hg --cwd %s locate -I .'],
"  	\   },
"  	\   'fallback': 'find %s -type f'
" 	\ }


Plug 'Valloric/YouCompleteMe', { 'do': './install.sh' }
let g:ycm_key_invoke_completion = '<C-K>'
let g:ycm_key_list_select_completion = ['<Down>', '<C-N>']
let g:ycm_key_list_previous_completion = ['<UP>', '<C-P>']
let g:ycm_autoclose_preview_window_after_insertion=1
" let g:ycm_autoclose_preview_window_after_completion=1
" autocmd CompleteDone * pclose

Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
let g:UltiSnipsExpandTrigger="<C-J>"
let g:UltiSnipsJumpForwardTrigger="<C-N>"
let g:UltiSnipsJumpBackwardTrigger="<C-P>"

Plug 'Shougo/unite.vim'
Plug 'tsukkee/unite-tag'

Plug 'tpope/vim-eunuch'

Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'scrooloose/syntastic'
let g:syntastic_check_on_open=1
let g:syntastic_auto_loc_list=1								" open and close the error list automatically

let g:syntastic_mode_map={
			\ 'mode': 'active',
      \ 'active_filetypes': [],
      \ 'passive_filetypes': ['html', 'lua']
			\ }

let g:syntastic_error_symbol = '✗'						" Better :sign interface symbols
let g:syntastic_warning_symbol = '!'

let g:syntastic_javascript_checkers = ['jshint']
let g:syntastic_lua_checkers = ['luainspect']

Plug 'rking/ag.vim'
Plug 'tpope/vim-unimpaired'
Plug 'Lokaltog/vim-easymotion'
map <Leader> <Plug>(easymotion-prefix)

Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-surround'

Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-markdown'

Plug 'flazz/vim-colorschemes'
Plug 'chriskempson/base16-vim'

Plug 'jordwalke/VimAutoMakeDirectory'

Plug 'vim-scripts/TaskList.vim'

Plug 'xolox/vim-misc'
Plug 'xolox/vim-easytags'
let g:easytags_cmd = '/usr/local/bin/ctags'   " Where to find ctags?
let g:easytags_async = 1                      " Run ctags asynchronously
let g:easytags_always_enabled = 0             " Don't always run
let g:easytags_auto_highlight = 0             " Don't try to highlight tags. Unacceptably slow on larger projects.
let g:easytags_dynamic_files = 1              " Generate tags local to a project
set tags=./.git/tags;                         " Where to store tag files local to the project

Plug 'godlygeek/tabular'

Plug 'moll/vim-bbye'

Plug 'shime/vim-livedown'
let g:livedown_autorun = 1
let g:livedown_open = 1
let g:livedown_port = 2000

Plug 'mattn/emmet-vim'
Plug 'xsbeats/vim-blade'
Plug 'vim-scripts/matchit.zip'
Plug 'zah/nimrod.vim'
Plug 'groenewege/vim-less'
Plug 'nacitar/a.vim'

Plug 'nathanaelkane/vim-indent-guides'

Plug 'junegunn/goyo.vim'

Plug 'rizzatti/dash.vim'
:nmap <silent> <leader>d <Plug>DashSearch

call plug#end()
filetype plugin indent on

source ~/.vimrc.after
