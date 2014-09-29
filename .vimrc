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
NeoBundle 'Shougo/vimproc.vim', {
      \ 'build' : {
      \     'windows' : 'tools\\update-dll-mingw',
      \     'cygwin' : 'make -f make_cygwin.mak',
      \     'mac' : 'make -f make_mac.mak',
      \     'unix' : 'make -f make_unix.mak',
      \    },
      \ }

NeoBundle 'Shougo/neosnippet.vim'
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'Shougo/vimshell'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'kien/ctrlp.vim'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'Lokaltog/powerline'
NeoBundle 'vim-scripts/Smart-Tabs'
NeoBundle 'chrisbra/SudoEdit.vim'
NeoBundle 'rking/ag.vim'
NeoBundle 'tpope/vim-unimpaired'
NeoBundle 'Lokaltog/vim-easymotion'
NeoBundle 'terryma/vim-multiple-cursors'
NeoBundle 'tpope/vim-surround'

NeoBundle 'tomtom/tcomment_vim'

NeoBundle 'flazz/vim-colorschemes'
NeoBundle 'chriskempson/base16-vim'

let g:neocomplete#enable_at_startup = 1
NeoBundle 'Shougo/neocomplete.vim'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ~> NeoBundle finish initialization

call neobundle#end()                                       " NeoBundle, required
filetype plugin indent on                                  " NeoBundle, required

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck

source ~/.vimrc.after
