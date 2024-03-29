-----------------------------------------------------------
-- Neovim API aliases
-----------------------------------------------------------

local cmd	= vim.cmd				-- execute Vim commands
local exec	= vim.api.nvim_exec		-- execute Vimscript
local fn	= vim.fn				-- call Vim functions
local g		= vim.g					-- global variables
local opt	= vim.opt				-- global/buffer/windows-scoped options
local api	= vim.api
local u		= require("util")

local runningNeovide = false
if g.neovide then
	runningNeovide = true
end

-----------------------------------------------------------
-- General
-----------------------------------------------------------
u.nnoremap('<SPACE>', '<Nop>')
g.mapleader      = " "    -- Leader key is Space
g.maplocalleader = ","    -- Local leader is comma

opt.termguicolors = true

opt.ttimeoutlen = 100
opt.timeoutlen  = 300
-- cmd("set clipboard+=unnamedplus")
opt.clipboard = "unnamedplus"

-- interval for writing swap file to disk, also used by gitsigns
opt.undofile = true
opt.updatetime = 250

-----------------------------------------------------------
-- Color scheme
-----------------------------------------------------------
g.background = "dark"
--g.colors_name = "gruvbox-material"
vim.cmd([[colorscheme gruvbox-material]])
-- vim.cmd([[colorscheme gruvbox]])

-----------------------------------------------------------
-- Basic settings
-----------------------------------------------------------
opt.relativenumber = false    -- Show relative line numbers
opt.number         = true     -- Show line numbers
opt.numberwidth = 2
opt.ruler = false

opt.cursorline     = true     -- highlight current line

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

opt.mouse = "a"

-- Remove Search Highlighting on esc key
u.nnoremap('<esc>', ':nohlsearch<return><esc>')

opt.guifont = "FiraCode Nerd Font:h13"

u.nnoremap('<c-v>', '"+p')
u.nnoremap('<D-v>', '"+p')

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

g.laststatus = 3		  		-- Always show statusline
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

u.noremap('j', 'gj')
u.noremap('k', 'gk')

-- Navigate windows faster
u.nnoremap("<C-H>", "<C-W>h")
u.nnoremap("<C-J>", "<C-W>j")
u.nnoremap("<C-K>", "<C-W>k")
u.nnoremap("<C-L>", "<C-W>l")
if g.nvim then
	u.nnoremap("<bs>", "<C-W>h")
end

u.tnoremap("<C-H>", [[<C-\><C-n><C-w>h]])
u.tnoremap("<C-J>", [[<C-\><C-n><C-w>j]])
u.tnoremap("<C-K>", [[<C-\><C-n><C-w>k]])
u.tnoremap("<C-L>", [[<C-\><C-n><C-w>l]])
u.tnoremap("<ESC>", [[<C-\><C-n>]])

u.nnoremap("<C-P>", "<cmd>lua require'odie.telescope'.find_files()<cr>")
u.nnoremap("Q", "<Nop>")

u.nnoremap("<leader>bda", "<cmd>call DeleteHiddenBuffers()<cr>")

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
-- cmd([[
-- 	" don't give |ins-completion-menu| messages.
-- 	set shortmess+=c
-- ]])

-- disable nvim intro
opt.shortmess:append "sI"

-- always show signcolumns
opt.signcolumn = "yes"


-----------------------------------------------------------
-- "chmod u+x" on shebang files
-----------------------------------------------------------
vim.api.nvim_create_autocmd("BufWritePost", {
	group = vim.api.nvim_create_augroup("AutoSetExeBit", {clear = true}),
	callback = function()
		local line = vim.api.nvim_buf_get_lines(0, 0, 1, false)[1]
		if (string.find(line, "^#!")) then
			vim.fn.jobstart({"chmod", "u+x", vim.fn.expand('%:p')})
		end
	end
})

