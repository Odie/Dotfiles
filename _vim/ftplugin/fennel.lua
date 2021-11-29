local wk = require("which-key")

wk.register({
	e = {
		name = 'Eval' ,
		['!'] = 'Eval & Replace',

		b = 'Eval buffer',
		e = 'Eval current form',
		E = 'Eval selection',
		r = 'Eval top level form',
		m = 'Eval at mark',
		w = 'Eval word',
	},

	l = {
		name = 'log' ,
		r = 'Soft Reset',
		R = 'Hard Reset',
		s = 'in split',
		t = 'in tab',
		v = 'in vertical split',
		q = 'quit',
	}
}, { prefix = "<localleader>", buffer = vim.api.nvim_get_current_buf()})
