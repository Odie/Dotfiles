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

""" ":Listmaps" lists all mappings in all sourced files in a separate buffer
Plug 'vim-scripts/listmaps.vim'

""" Better looking status line
Plug 'bling/vim-airline'
let g:airline_powerline_fonts = 1

""" Try to use tabs where possible
Plug 'Odie/Smart-Tabs'

""" control-p to jump between files
""" Commandline utility written in Go
""" Requires separate installation
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes \| ./install' }
nnoremap <silent> <C-P> :FZF<CR>

""" Auto-completion
Plug 'Valloric/YouCompleteMe', { 'do': './install.sh' }
let g:ycm_key_invoke_completion = '<C-K>'
let g:ycm_key_list_select_completion = ['<Down>', '<C-N>']
let g:ycm_key_list_previous_completion = ['<UP>', '<C-P>']
let g:ycm_autoclose_preview_window_after_insertion=1
" let g:ycm_autoclose_preview_window_after_completion=1
" autocmd CompleteDone * pclose

""" Snippets
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
let g:UltiSnipsExpandTrigger="<C-J>"
let g:UltiSnipsJumpForwardTrigger="<C-N>"
let g:UltiSnipsJumpBackwardTrigger="<C-P>"

""" Manage and jump between buffers
""" Note: Consider removing "unite" related plugins
""" It was mostly used to jump back to previous buffers.
Plug 'Shougo/unite.vim'
Plug 'tsukkee/unite-tag'

""" Unix helpers
""" ":SudoWrite" to write a file with sudo
""" Other available commands:
"""   :Remove, :Unlink, :Move, Rename, :Chmod, :Mkdir, :Find, :Locate,
"""   :Wall, :SudoEdit
Plug 'tpope/vim-eunuch'

""" File explorer
""" "<Leader>n" to toggle on and off
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }

""" Syntax highlighting
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

""" Better search support via ag
""" ":Ag" and ":Ags" to search
Plug 'rking/ag.vim'
Plug 'gabesoft/vim-ags'

""" Adds various ex command shortcuts
""" [b and ]b to switch to previous and next buffer
Plug 'tpope/vim-unimpaired'

""" Jump to location in buffer faster
Plug 'Lokaltog/vim-easymotion'
map <Leader> <Plug>(easymotion-prefix)

Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-surround'

""" Quickly comment/uncomment lines
Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-repeat'

""" Color schemes
Plug 'flazz/vim-colorschemes'
Plug 'chriskempson/base16-vim'

""" Automatically make containing directory when opening a new buffer
Plug 'jordwalke/VimAutoMakeDirectory'

""" Generate tasklist by searching for FIXME, TODO, and XXX
Plug 'vim-scripts/TaskList.vim'

Plug 'xolox/vim-misc'

""" Automate tag file generation
Plug 'xolox/vim-easytags'
let g:easytags_cmd = '/usr/local/bin/ctags'   " Where to find ctags?
let g:easytags_async = 1                      " Run ctags asynchronously
let g:easytags_always_enabled = 0             " Don't always run
let g:easytags_auto_highlight = 0             " Don't try to highlight tags. Unacceptably slow on larger projects.
let g:easytags_dynamic_files = 1              " Generate tags local to a project
set tags=./.git/tags;                         " Where to store tag files local to the project

""" Shortcuts for aligning text
Plug 'godlygeek/tabular'
:vmap <silent> <Leader>a=  :Tabularize /= <cr>
:vmap <silent> <Leader>a:  :Tabularize /: <cr>
:vmap <silent> <Leader>a:: :Tabularize /:\zs <cr>
:vmap <silent> <Leader>a,  :Tabularize /, <cr>
:vmap <silent> <Leader>a<Bar> :Tabularize /

""" "<Leader> q" quits the buffer
Plug 'moll/vim-bbye'

""" Show live Markdown preview in browser
Plug 'shime/vim-livedown'
let g:livedown_autorun = 1
let g:livedown_open = 1
let g:livedown_port = 2000

""" Html structure generation
Plug 'mattn/emmet-vim'

""" Matches opening and closing html tags
Plug 'vim-scripts/matchit.zip'

""" Switch to alternative files quickly
Plug 'nacitar/a.vim'

""" Visually display indent levels
""" "<Leader> ig" to toggle on/off
Plug 'nathanaelkane/vim-indent-guides'

""" Distraction free writing in vim
""" ":Goyo" to toggle
Plug 'junegunn/goyo.vim'

""" Search documentation in Dash.app
Plug 'rizzatti/dash.vim'
:nmap <silent> <leader>d <Plug>DashSearch

""" Git integration with vim
Plug 'tpope/vim-fugitive'



"-------------------------------------------------------------------------------
" Syntax highlighting
"-------------------------------------------------------------------------------

""" Python syntax support
Plug 'hdima/python-syntax'

""" Syntax highlighting for blade templating language
Plug 'xsbeats/vim-blade'

""" Syntax highlighting for nimrod files
Plug 'zah/nimrod.vim'

""" Syntax highlighting for less files
Plug 'groenewege/vim-less'

""" Syntax highlighting for markdown files
Plug 'tpope/vim-markdown'

""" Python Linter
""" Requires flake8
Plug 'nvie/vim-flake8'

call plug#end()
filetype plugin indent on

source ~/.vimrc.after
