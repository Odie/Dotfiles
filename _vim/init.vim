if has('vim_starting')
 	set nocompatible                                         " Be iMproved
endif

set termguicolors

"-------------------------------------------------------------------------------
" Setup leader key
"-------------------------------------------------------------------------------
nnoremap <SPACE> <Nop>
let mapleader = " "
let maplocalleader = ","

"-------------------------------------------------------------------------------
" Load up all of our plugins
"-------------------------------------------------------------------------------
if filereadable(expand("~/.vim/init.bundles.vim"))
  source ~/.vim/init.bundles.vim
endif

lua require("plugins")
augroup AutoCompilePlugins
  au!
  autocmd BufWritePost plugins.lua PackerCompile
augroup end

lua require("config/start")
let g:gitabra_dev = 1

set ttimeoutlen=100
set timeoutlen=500

nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>
nnoremap <silent> <localleader> :<c-u>WhichKey  ','<CR>
vnoremap <silent> <leader> :<c-u>WhichKeyVisual '<Space>'<CR>
vnoremap <silent> <localleader> :<c-u>WhichKeyVisual ','<CR>

call which_key#register('<Space>', "g:which_key_map")
let g:which_key_map =  {}

let g:which_key_map.b = { 'name' : '+buffer' }
let g:which_key_map.g = { 'name' : '+git' }
let g:which_key_map.f = { 'name' : '+files' }
let g:which_key_map.f.e = { 'name' : '+editor' }
let g:which_key_map.h = { 'name' : '+help' }
let g:which_key_map.t = { 'name' : '+toggle' }

call which_key#register(',', "g:which_key_local_map")
let g:which_key_local_map =  {}

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ~> Color Scheme

set background=dark
color gruvbox-material

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

" Don't bother setting the cursorline if we're running neovide
" The animated cursor makes is very clear where it is on the screen.
if !exists('g:neovide')
	set cursorline          " highlight current line
endif

syntax on               " Always turn on syntax highlighting
set title               " Sets terminal title

set backspace=indent,eol,start      " Erase autoindents, join lines

set autoread            " Reload file when modified
set hidden              " Hide buffers instead of closing them

set listchars+=tab:▸\ ,trail:·				" How to show invisible characters
set showbreak=⇇
hi SpecialKey ctermfg=243 guifg=gray	" What color to show them in


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ~> Search behaviors

set incsearch           " Always do incremental search
set hlsearch            " Highlight search results
set ignorecase          " Search is case-insensitive
set smartcase           " Unless we type in a caps letter somewhere

" Remove Search Highlighting on esc key
nnoremap <silent> <esc> :nohlsearch<return><esc>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ~> GUI settings

set guifont=FiraCodeNerdFontComplete-Regular:h13


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
augroup NoVisualBell
	au!
	autocmd GUIEnter * set visualbell t_vb=
augroup END

set ttyfast
set lazyredraw

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ~> Swapfile settings
" Centrally store swap files
set nobackup
set directory=~/.vim/tmp

"=====[ Use par for better formatting ]=============
set formatprg=par

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


"[Plugin Settings]======================================{{{

"=====[ Powerline Settings ]=============
set laststatus=2		  " Always show statusline
" set t_Co=256			    " Use 256 colours (Use this setting only if your terminal supports 256 colours)
set noshowmode			  " Hide the default mode text (e.g. -- INSERT -- below the statusline)
set encoding=utf-8		" make sure we can display fancy characters correctly

" Easymotion: Make sure we can see the first key in a 2 key target
hi link EasyMotionTarget2First EasyMotionTargetDefault

let g:indent_guides_auto_colors = 0
" autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=DarkGray ctermbg=DarkGray

"=====[ Emmet.vim settings ]=============
let g:user_emmet_mode='a'                   "enable all function in all mode.
let g:user_emmet_install_global = 0
augroup EnableEmmet
	au!
	autocmd FileType html,css,hbs,blade EmmetInstall  "enable for specific file types only
augroup END

"=====[ Clojure settings ]=============
" Nothing here

"=====[ Nim settings ]=============
augroup filetype_detect
	autocmd!
	autocmd BufRead,BufNewFile *.nim setlocal filetype=nim
augroup END
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
function! s:MaybeMiddle()
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

function DeleteHiddenBuffers()
    let tpbl=[]
    call map(range(1, tabpagenr('$')), 'extend(tpbl, tabpagebuflist(v:val))')
    for buf in filter(range(1, bufnr('$')), 'bufexists(v:val) && index(tpbl, v:val)==-1')
        silent execute 'bwipeout' buf
    endfor
endfunction
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


" Navigate windows faster
nnoremap <C-H> <C-W>h
nnoremap <C-J> <C-W>j
nnoremap <C-K> <C-W>k
nnoremap <C-L> <C-W>l
if has('nvim')
	nnoremap <bs> <C-W>h
endif

" Navigate window in terminal mode
tnoremap <C-H> <C-\><C-n><C-w>h
tnoremap <C-J> <C-\><C-n><C-w>j
tnoremap <C-K> <C-\><C-n><C-w>k
tnoremap <C-L> <C-\><C-n><C-w>l

" Better popup navigation
inoremap <expr> <C-K> pumvisible() ? "<C-p>" : "<C-K>"
inoremap <expr> <C-J> pumvisible() ? "<C-n>" : "<C-J>"
cnoremap <expr> <C-K> pumvisible() ? "<C-p>" : "<C-K>"
cnoremap <expr> <C-J> pumvisible() ? "<C-n>" : "<C-J>"

nnoremap <silent> <C-P> <cmd>lua require'odie.telescope'.find_files()<cr>


" Toggle paste mode
" nnoremap <leader>p :set invpaste paste?<CR>

" Don't enter Ex mode
nnoremap Q <Nop>

" Project-wide Search
nnoremap <silent> <Leader>/ :call esearch#init()<cr>
let g:which_key_map['/'] = 'Search project'

let g:which_key_map.n = ['NvimTreeToggle', 'File tree']

"=====[ Buffer maniulation ]=============
nnoremap <silent> <leader><tab> <c-^>
" let g:which_key_map['<TAB>'] = 'Last Buffer'

nnoremap <leader>bb <cmd>Telescope buffers<cr>
nnoremap <leader>bda <cmd>call DeleteHiddenBuffers()<cr>
let g:which_key_map.b.b = 'Buffers'
let g:which_key_map.b.d = ['BufferClose', 'Delete Buffer']
let g:which_key_map.b.D = ['BufferClose!', 'Delete Buffer (force)']
let g:which_key_map.b.p = ['bprev', 'Next buffer']
let g:which_key_map.b.n = ['bnext', 'Prev buffer']

"=====[ File manipulation ]=============
nnoremap <leader>fed :e $MYVIMRC<cr>
let g:which_key_map.f.e.d = 'Open config file'

nnoremap <silent> <leader>feR :source $MYVIMRC<cr>:echom "Sourced vimrc"<cr>
let g:which_key_map.f.e.R = 'Reload config file'

nnoremap <leader>ff <cmd>lua require'odie.telescope'.find_files()<cr>
let g:which_key_map.f.f = 'Find by name'

nnoremap <leader>fg <cmd>lua require('telescope.builtin').live_grep({find_command = require'odie.telescope'.find_command})<cr>
let g:which_key_map.f.g = 'Grep in file'

let g:which_key_map.f.t = ['NvimTreeToggle', 'File tree']


"=====[ Git ]=============
nnoremap <leader>gb <cmd>Telescope git_branches<CR>
let g:which_key_map.g.b = 'Switch Branch'

nnoremap <leader>gB :Gblame<CR>
nnoremap <silent> <leader>gs :Gitabra<CR>
nnoremap <leader>gd :Gdiff<CR>
nnoremap <leader>gl :Glog<CR>
nnoremap <leader>gc :Gcommit<CR>
nnoremap <leader>gp :Gpush<CR>
nnoremap <leader>ht <cmd>Telescope help_tags<cr>

"=====[ help ]=============
let g:which_key_map.h.t = 'Find Tags'

"=====[ Highlight matches when jumping to next ]=============
nnoremap <silent> n n:call <SID>MaybeMiddle()<cr>:call HLNext(0.125)<cr>
nnoremap <silent> N N:call <SID>MaybeMiddle()<cr>:call HLNext(0.125)<cr>

"=====[ Setup movement mappings ]=============
"" Select contents of next or last ()
onoremap in( :<c-u>normal! f(vi(<cr>
onoremap il( :<c-u>normal! F)vi(<cr>


"=====[ Completion ]=============
" You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=300

" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.

"=====[ Clojure specific ]=============
" nnoremap ,' :IcedConnect<cr>
" nnoremap ,sq :IcedDisconnect<cr>
" nnoremap ,sn :IcedEvalNs<cr>
" nnoremap ,ef :IcedEvalOuterTopList<cr>
" nnoremap ,ee <Plug>(iced_eval_and_print)<Plug>(sexp_outer_list)``
"

let g:which_key_map.t.i = ['IndentGuidesToggle', "Indent guide"]

let g:which_key_map.t.l = ['set wrap!', "Line wrap"]
nnoremap <leader>tl :set wrap!<cr>

let g:which_key_map.q = [':close', 'Close window']

nnoremap <leader>ntn :tabnext<cr>
nnoremap <leader>ntp :tabprev<cr>
"=================================================================}}}


highlight TelescopeSelection      guibg=#3C3836

" augroup AttachCompletion
" 	autocmd!
" 	autocmd BufEnter * lua require'completion'.on_attach()
" augroup END
