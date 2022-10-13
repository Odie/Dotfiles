vim.opt.expandtab = true
vim.o.tabstop = 4

local core = require("iron.core")
local ft = vim.bo.filetype

local wk = require("which-key")
wk.register({
	e = {
		name = 'Eval' ,
		b = {function() core.send_file(ft) end, 'Eval buffer'},
		l = {function() core.send_line(ft) end, 'Eval current line'},
		v = {function() core.visual_send(ft) end, 'Eval selection'},
	},

	s = {
		name = 'repl' ,
		a = {function() core.focus_on(ft) end, 'Switch to repl'},
		h = {function() core.hide_repl(ft) end, 'Hide'},
		r = {function() core.repl_restart(ft) end, 'Hard Reset'},
		q = {function() core.close_repl(ft) end, 'Quit'},
		c = {function() core.send(nil, string.char(12)) end, 'Clear'},
		['<cr>'] = {function() core.send(nil, string.char(13)) end, 'cr'},
		['<space>'] = {function() core.send(nil, string.char(03)) end, 'interrupt'}
	}
}, { prefix = "<localleader>", buffer = vim.api.nvim_get_current_buf()})

wk.register({
	e = {
		v = {function() core.visual_send(ft) end, 'Eval selection'},
	},

}, { mode = "v", prefix = "<localleader>", buffer = vim.api.nvim_get_current_buf()})
