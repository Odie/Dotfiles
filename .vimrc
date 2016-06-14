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
" Load up all of our plugins
"-------------------------------------------------------------------------------
if filereadable(expand("~/.vimrc.bundles"))
  source ~/.vimrc.bundles
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ~> Color Scheme
let $NVIM_TUI_ENABLE_TRUE_COLOR=1

set background=dark
color gruvbox

let g:terminal_color_0="#353535"
let g:terminal_color_1="#d73a26"
let g:terminal_color_2="#a8a522"
let g:terminal_color_3="#dfa82a"
let g:terminal_color_4="#54979a"
let g:terminal_color_5="#c07998"
let g:terminal_color_6="#79ab7d"
let g:terminal_color_7="#c9bca4"

let g:terminal_color_8="#8f8277"
let g:terminal_color_9="#fc6143"
let g:terminal_color_10="#c5c431"
let g:terminal_color_11="#fcc73c"
let g:terminal_color_12="#95b3a9"
let g:terminal_color_13="#dd9aab"
let g:terminal_color_14="#9eca8e"
let g:terminal_color_15="#efe1bf"

let g:terminal_color_background="#282828"
let g:terminal_color_foreground="#ebdca8"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ~> Basic Settings
set relativenumber      " Show relative line numbers
set number              " Show line numbers
set cursorline          " highlight current line

syntax on               " Always turn on syntax highlighting
set title               " Sets terminal title

set backspace=indent,eol,start      " Erase autoindents, join lines

set autoread            " Reload file when modified
set hidden              " Hide buffers instead of closing them

set listchars+=tab:▸\ ,trail:·				" How to show invisible characters
set showbreak=⇇
hi SpecialKey ctermfg=243 guifg=gray	" What color to show them in

set ttimeoutlen=100
set timeoutlen=3000

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ~> Search behaviors

set incsearch           " Always do incremental search
set hlsearch            " Highlight search results
set ignorecase          " Search is case-insensitive
set smartcase           " Unless we type in a caps letter somewhere

" Remove Search Highlighting on esc key
if has('gui')
  nnoremap <silent> <esc> :nohlsearch<return><esc>
else
	nnoremap <esc> :noh<return><esc>
endif



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ~> GUI settings

if has('gui_running')
	set guifont=Anonymous\ Pro\ for\ Powerline:h14
	set columns=999    " create windows with maximum width by default
	set lines=999      " create windows with maximum height by default
endif



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ~> Tab settings

set noexpandtab
set copyindent
set smartindent
set preserveindent
set softtabstop=0
set shiftwidth=2
set tabstop=2
"augroup detect_indent
"	autocmd!
"	autocmd BufReadPost * :DetectIndent
"	let g:detectindent_preferred_indent = 2
"augroup END



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ~> Clipboard settings

"Enable clipboard in terminal
if ! has('gui_running')
	set clipboard=unnamed
endif

"Disable bells
set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=

set ttyfast
set lazyredraw

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ~> Swapfile settings
" Centrally store swap files
set nobackup
set directory=~/.vim/tmp

"=====[ Use par for better formatting ]=============
:set formatprg=par

"=====[ Emmet.vim settings ]=============
let g:user_emmet_mode='a'                   "enable all function in all mode.
let g:user_emmet_install_global = 0
autocmd FileType html,css,hbs,blade EmmetInstall  "enable for specific file types only
"let g:user_emmet_leader_key='<C-I>'

"==================================================================}}}

augroup no_trailing_spaces
	" By default, trailing spaces will be trimmed for all file types.
	" Space trimming will be applied on BufWrite.
	" This can be disabled on a buffer to buffer basis by setting
	" b:trim_trailing_spaces = 0
	autocmd!
	autocmd BufNewFile,BufRead *
		\ if !exists("b:trim_trailing_spaces")  |
		\   let b:trim_trailing_spaces=1        |
		\ endif

	autocmd BufWrite *
		\ if (! &bin) && b:trim_trailing_spaces |
		\		silent! %s/\s\+$//ge |
		\ endif
augroup END

augroup filetype_detect
	autocmd!
	autocmd BufRead,BufNewFile *.nim setlocal filetype=nim
augroup END

"[Plugin Settings]======================================{{{

"=====[ Powerline Settings ]=============
set laststatus=2		  " Always show statusline
set t_Co=256			    " Use 256 colours (Use this setting only if your terminal supports 256 colours)
set noshowmode			  " Hide the default mode text (e.g. -- INSERT -- below the statusline)
set encoding=utf-8		" make sure we can display fancy characters correctly

" Easymotion: Make sure we can see the first key in a 2 key target
hi link EasyMotionTarget2First EasyMotionTargetDefault

let g:indent_guides_auto_colors = 0
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=DarkGray ctermbg=DarkGray

"==================================================================}}}

"[Custom functions]================================================{{{

"======================================================================
" Highlight the match in red
function! HLNext (totalBlinkTime)
	highlight WhiteOnRed ctermfg=white ctermbg=red guibg=darkred guifg=white
	let [bufnum, lnum, col, off] = getpos('.')
	let matchlen = strlen(matchstr(strpart(getline('.'),col-1),@/))
	let target_pat = '\c\%#'.@/

	let blinkCount = 1
	let blinkTime = a:totalBlinkTime/((blinkCount * 2) - 1)
	let i = 0
	while i < blinkCount
		let i += 1
		let ring = matchadd("WhiteOnRed", target_pat, 101)
		redraw
		exec 'sleep ' . float2nr(blinkTime * 1000) . 'm'
		call matchdelete(ring)
		redraw
		if i < blinkCount
			exec 'sleep ' . float2nr(blinkTime * 1000) . 'm'
		endif
	endwhile
endfunction

"======================================================================
" If cursor is in first or last line of window, scroll to middle line.
function s:MaybeMiddle()
  if winline() == 1 || winline() == winheight(0)
    normal! zz
  endif
endfunction

" trim trailing whitespace in the current file
function! RTrim()
  %s/\v\s+$//e
  noh
endfunction

function! Stab()
  let l:tabstop = 1 * input('set tabstop = softtabstop = shiftwidth = ')
  if l:tabstop > 0
    let &l:sts = l:tabstop
    let &l:ts = l:tabstop
    let &l:sw = l:tabstop
  endif
  call SummarizeTabs()
endfunction

function! SummarizeTabs()
  try
    echohl ModeMsg
    echon 'tabstop='.&l:ts
    echon ' shiftwidth='.&l:sw
    echon ' softtabstop='.&l:sts
    if &l:et
      echon ' expandtab'
    else
      echon ' noexpandtab'
    endif
  finally
    echohl None
  endtry
endfunction

command! -nargs=1 -range SuperRetab <line1>,<line2>s/\v%(^ *)@<= {<args>}/\t/g
"==================================================================}}}

"[Mappings]=========================================================={{{

"=====[ Faster ':' commands ]=============

" Switch ":" and ";"
nnoremap ; :
nnoremap : ;

" kj to escape insert mode
inoremap <special> kj <Esc>

" kj move between visual lines
noremap j gj
noremap k gk

" Shortcut to regenerate tags
nnoremap <leader><leader>rt :UpdateTags -R .<CR>

" Open NerdTree quickly
nnoremap <leader>n :NERDTreeToggle<CR>

" Unite keys
nnoremap <leader><C-p> :Unite -no-split -auto-preview -start-insert tag<CR>
nnoremap <leader>r :Unite -buffer-name=mru		 file_mru<cr>
nnoremap <leader>o :Unite -buffer-name=outline outline<cr>
nnoremap <leader>y :Unite -buffer-name=yank		 history/yank<cr>
nnoremap <leader>b :Unite -buffer-name=buffer -quick-match buffer<cr>
"nnoremap <C-p> :Unite file_rec/async<CR>
"nnoremap <C-i> :Unite -buffer-name=files	file<cr>

" Fugitive keys
nnoremap <leader>gb :Gblame<CR>
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gd :Gdiff<CR>
nnoremap <leader>gl :Glog<CR>
nnoremap <leader>gc :Gcommit<CR>
nnoremap <leader>gp :Gpush<CR>

" Navigate windows faster
nnoremap <C-H> <C-W>h
nnoremap <C-J> <C-W>j
nnoremap <C-K> <C-W>k
nnoremap <C-L> <C-W>l
if has('nvim')
	nnoremap <bs> <C-W>h
endif

" Toggle paste mode
nnoremap <leader>p :set invpaste paste?<CR>

" Quickly delete a buffer without altering layout
nnoremap <Leader>q :Bdelete<CR>

" Don't enter Ex mode
:nnoremap Q <Nop>

"=====[ Setup key mappings ]=============
:nnoremap <leader>ev :vsplit ~/.vimrc<cr>
:nnoremap <leader>sv :source ~/.vimrc<cr>:echom "Sourced vimrc"<cr>

"=====[ Highlight matches when jumping to next ]=============
nnoremap <silent> n n:call <SID>MaybeMiddle()<cr>:call HLNext(0.125)<cr>
nnoremap <silent> N N:call <SID>MaybeMiddle()<cr>:call HLNext(0.125)<cr>

"=====[ Setup movement mappings ]=============
"" Select contents of next or last ()
:onoremap in( :<c-u>normal! f(vi(<cr>
:onoremap il( :<c-u>normal! F)vi(<cr>

"==================================================================}}}
