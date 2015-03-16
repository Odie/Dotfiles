""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ~> NeoBundle initialization

if has('vim_starting')
  set nocompatible                                         " Be iMproved
  set runtimepath+=~/.vim/bundle/neobundle.vim/            " NeoBundle, required
endif

source ~/.vimrc.before

call neobundle#begin(expand('~/.vim/bundle'))              " NeoBundle, required

" Let NeoBundle manage NeoBundle
NeoBundleFetch 'Shougo/neobundle.vim'                      " NeoBundle, required



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ~> Bundle definitions
" Add or remove your Bundles here:

NeoBundle 'bling/vim-airline'
let g:airline_powerline_fonts = 1

NeoBundle 'Shougo/vimproc.vim', {
      \ 'build' : {
      \     'windows' : 'tools\\update-dll-mingw',
      \     'cygwin' : 'make -f make_cygwin.mak',
      \     'mac' : 'make -f make_mac.mak',
      \     'unix' : 'make -f make_unix.mak',
      \    },
      \ }

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ~> Bundle: Auto-completion

let g:neocomplete#enable_at_startup = 1
NeoBundle 'Shougo/neocomplete.vim'

NeoBundle 'SirVer/ultisnips'
NeoBundle 'Odie/Smart-Tabs'

" Use <tab> and <s-tab> to expand a snippet and move between fields
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

" Integrate SmartTab and Ultisnip
let g:ulti_expand_or_jump_res = 0
function! Ulti_ExpandOrJump_and_getRes()
 call UltiSnips#ExpandSnippetOrJump()
 return g:ulti_expand_or_jump_res
endfunction

if neobundle#tap('ultisnips')
	function! neobundle#hooks.on_post_source(bundle)
		inoremap <silent> <tab> <C-R>=(Ulti_ExpandOrJump_and_getRes() > 0)?"":InsertSmartTab()<CR>
	endfunction
	call neobundle#untap()
endif

NeoBundle 'honza/vim-snippets'

NeoBundle 'Shougo/vimshell'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'tsukkee/unite-tag'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-eunuch'

NeoBundle 'kien/ctrlp.vim'
let g:ctrlp_follow_symlinks = 2   " Always follow symlinks
let g:ctrlp_user_command = {
	\ 'types': {
 	\     1: ['.git', 'cd %s && git ls-files --cached --exclude-standard --others'],
 	\     2: ['.hg', 'hg --cwd %s locate -I .'],
 	\   },
 	\   'fallback': 'find %s -type f'
  \ }

NeoBundle 'scrooloose/nerdtree'

NeoBundle 'scrooloose/syntastic'

NeoBundle 'Lokaltog/powerline'
NeoBundle 'rking/ag.vim'
NeoBundle 'tpope/vim-unimpaired'
NeoBundle 'Lokaltog/vim-easymotion'
map <Leader> <Plug>(easymotion-prefix)

NeoBundle 'terryma/vim-multiple-cursors'
NeoBundle 'tpope/vim-surround'

NeoBundle 'tomtom/tcomment_vim'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-markdown'

NeoBundle 'flazz/vim-colorschemes'
NeoBundle 'chriskempson/base16-vim'

NeoBundle 'jordwalke/VimAutoMakeDirectory'

NeoBundle 'vim-scripts/TaskList.vim'

NeoBundle 'xolox/vim-misc'
NeoBundle 'xolox/vim-easytags'
let g:easytags_cmd = '/usr/local/bin/ctags'   " Where to find ctags?
let g:easytags_async = 1                      " Run ctags asynchronously
let g:easytags_always_enabled = 0             " Don't always run
let g:easytags_auto_highlight = 0             " Don't try to highlight tags. Unacceptably slow on larger projects.
let g:easytags_dynamic_files = 1              " Generate tags local to a project
set tags=./.git/tags;                  " Where to store tag files local to the project

NeoBundle 'godlygeek/tabular'

NeoBundle 'moll/vim-bbye'

NeoBundle 'suan/vim-instant-markdown'

NeoBundle 'mattn/emmet-vim'
NeoBundle 'xsbeats/vim-blade'
NeoBundle 'vim-scripts/matchit.zip'
NeoBundle 'zah/nimrod.vim'
NeoBundle 'groenewege/vim-less'
NeoBundle 'nacitar/a.vim'

NeoBundle 'nathanaelkane/vim-indent-guides'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ~> NeoBundle finish initialization

call neobundle#end()                                       " NeoBundle, required
filetype plugin indent on                                  " NeoBundle, required

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck

source ~/.vimrc.after
