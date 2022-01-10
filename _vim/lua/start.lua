-----------------------------------------------------------
-- Neovim API aliases
-----------------------------------------------------------

--local map = vim.api.nvim_set_keymap  -- set global keymap
local cmd   = vim.cmd     				-- execute Vim commands
local exec  = vim.api.nvim_exec 	-- execute Vimscript
local fn    = vim.fn       			  -- call Vim functions
local g     = vim.g         			-- global variables
local opt   = vim.opt         		-- global/buffer/windows-scoped options
local api   = vim.api

function noremap(key, val)
	api.nvim_set_keymap('', key,  val, {noremap = true, silent = true})
end

function nnoremap(key, val)
	api.nvim_set_keymap('n', key,  val, {noremap = true, silent = true})
end

function vnoremap(key, val)
	api.nvim_set_keymap('v', key,  val, {noremap = true, silent = true})
end

function vnoremap(key, val)
	api.nvim_set_keymap('i', key,  val, {noremap = true, silent = true})
end

function tnoremap(key, val)
	api.nvim_set_keymap('t', key,  val, {noremap = true, silent = true})
end


local runningNeovide = false
if g.neovide then
	runningNeovide = true
end


-----------------------------------------------------------
-- General
-----------------------------------------------------------
nnoremap('<SPACE>', '<Nop>')
g.mapleader      = " "    -- Leader key is Space
g.maplocalleader = ","    -- Local leader is comma

opt.termguicolors = true

opt.ttimeoutlen = 100
opt.timeoutlen  = 300

-----------------------------------------------------------
-- Color scheme
-----------------------------------------------------------
g.background = "dark"
--g.colors_name = "gruvbox-material"
cmd([[colorscheme gruvbox-material]])

-----------------------------------------------------------
-- Basic settings
-----------------------------------------------------------
opt.relativenumber = true     -- Show relative line numbers
opt.number         = true     -- Show line numbers

-- Don't bother setting the cursorline if we're running neovide
-- The animated cursor makes is very clear where it is on the screen.
if not runningNeovide then
	g.cursorline = true         -- highlight current line
end

-- syntax on               " Always turn on syntax highlighting
g.title     = true                 -- Sets terminal title
g.backspace = "indent,eol,start"   -- Erase autoindents, join lines

g.autoread = true                  -- Reload file when modified
g.hidden   = true                  -- Hide buffers instead of closing them

-- Showing invisible characters
cmd([[set listchars+=tab:▸\ ,trail:·]])				-- How to show invisible characters
cmd([[set showbreak=⇇]])
cmd([[hi SpecialKey ctermfg=243 guifg=gray]])	-- What color to show them in

opt.incsearch  = true          -- Always do incremental search
opt.hlsearch   = true          -- Highlight search results
opt.ignorecase = true          -- Search is case-insensitive
opt.smartcase  = true          -- Unless we type in a caps letter somewhere

-- Remove Search Highlighting on esc key
nnoremap('<esc>', ':nohlsearch<return><esc>')

opt.guifont = "FiraCode Nerd Font:h13"


-----------------------------------------------------------
-- Tab settings
-----------------------------------------------------------
opt.expandtab      = false
opt.copyindent     = true
opt.smartindent    = true
opt.preserveindent = true
opt.tabstop        = 4
opt.softtabstop    = 4
opt.shiftwidth     = 4

if not g.gui_running then
	g.clipboard = "unnamed"
end

-- Disable bells
cmd([[
	set noerrorbells visualbell t_vb=
	augroup NoVisualBell
		au!
		autocmd GUIEnter * set visualbell t_vb=
	augroup END
]])

g.ttyfast    = true
g.lazyredraw = true

-----------------------------------------------------------
-- Swapfile settings
-- Centrally store swap files
-----------------------------------------------------------
g.nobackup   = true
g.directory  = "~/.vim/tmp"

-- Use par for better formatting
g.formatprg  = "par"

cmd([[
	augroup no_trailing_spaces
		" By default, trailing spaces will be trimmed for all file types.
		" Space trimming will be applied on BufWrite.
		" This can be disabled on a buffer to buffer basis by setting
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
]])

g.laststatus = 2		  		-- Always show statusline
g.noshowmode = true			  -- Hide the default mode text (e.g. -- INSERT -- below the statusline)
g.encoding   = "utf-8"		-- make sure we can display fancy characters correctly

-- Easymotion: Make sure we can see the first key in a 2 key target
cmd([[hi link EasyMotionTarget2First EasyMotionTargetDefault]])


g.indent_guides_auto_colors = 0
-- autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=DarkGray ctermbg=DarkGray
--

-----------------------------------------------------------
-- Emmet settings
-----------------------------------------------------------
g.user_emmet_mode = 'a'                   -- enable all function in all mode.
g.user_emmet_install_global = 0

cmd([[
	augroup EnableEmmet
		au!
		autocmd FileType html,css,hbs,blade EmmetInstall  "enable for specific file types only
	augroup END
]])


-----------------------------------------------------------
-- File type settings
-----------------------------------------------------------
cmd([[
	augroup filetype_detect
		autocmd!
		autocmd BufRead,BufNewFile *.nim setlocal filetype=nim
	augroup END
]])

cmd([[
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
]])

cmd([[
	"======================================================================
	" If cursor is in first or last line of window, scroll to middle line.
	function! MaybeMiddle()
  	if winline() == 1 || winline() == winheight(0)
    	normal! zz
  	endif
	endfunction
]])

cmd([[
	" trim trailing whitespace in the current file
	function! RTrim()
  	%s/\v\s+$//e
  	noh
	endfunction
]])

cmd([[
	function! Stab()
  	let l:tabstop = 1 * input('set tabstop = softtabstop = shiftwidth = ')
  	if l:tabstop > 0
    	let &l:sts = l:tabstop
    	let &l:ts = l:tabstop
    	let &l:sw = l:tabstop
  	endif
  	call SummarizeTabs()
	endfunction
]])

cmd([[
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
]])

cmd([[
	command! -nargs=1 -range SuperRetab <line1>,<line2>s/\v%(^ *)@<= {<args>}/\t/g
]])

cmd([[
	function DeleteHiddenBuffers()
    	let tpbl=[]
    	call map(range(1, tabpagenr('$')), 'extend(tpbl, tabpagebuflist(v:val))')
    	for buf in filter(range(1, bufnr('$')), 'bufexists(v:val) && index(tpbl, v:val)==-1')
        	silent execute 'bwipeout' buf
    	endfor
	endfunction
]])

-- Switch ":" and ";"
--
cmd([[
	nnoremap ; :
	nnoremap : ;
	vnoremap ; :
	vnoremap : ;
]])

cmd([[inoremap <special> kj <Esc>]])

noremap('j', 'gj')
noremap('k', 'gk')

-- Navigate windows faster
nnoremap("<C-H>", "<C-W>h")
nnoremap("<C-J>", "<C-W>j")
nnoremap("<C-K>", "<C-W>k")
nnoremap("<C-L>", "<C-W>l")
if g.nvim then
	nnoremap("<bs>", "<C-W>h")
end

tnoremap("<C-H>", [[<C-\><C-n><C-w>h]])
tnoremap("<C-J>", [[<C-\><C-n><C-w>j]])
tnoremap("<C-K>", [[<C-\><C-n><C-w>k]])
tnoremap("<C-L>", [[<C-\><C-n><C-w>l]])

nnoremap("<C-P>", "<cmd>lua require'odie.telescope'.find_files()<cr>")
nnoremap("Q", "<Nop>")

nnoremap("<leader>bda", "<cmd>call DeleteHiddenBuffers()<cr>")

cmd([[
	nnoremap <silent> n n:call MaybeMiddle()<cr>:call HLNext(0.125)<cr>
	nnoremap <silent> N N:call MaybeMiddle()<cr>:call HLNext(0.125)<cr>
]])


-- Setup movement mappings
-- Select contents of next or last ()
cmd([[
	onoremap in( :<c-u>normal! f(vi(<cr>
	onoremap il( :<c-u>normal! F)vi(<cr>
]])


-----------------------------------------------------------
-- File type settings
-- You will have bad experience for diagnostic messages when it's default 4000.
-----------------------------------------------------------
cmd([[
	set updatetime=300

	" don't give |ins-completion-menu| messages.
	set shortmess+=c

	" always show signcolumns
	set signcolumn=yes
]])
