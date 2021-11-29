local wk = require("which-key")

local map = function(type, key, value)
	vim.fn.nvim_buf_set_keymap(0,type,key,value,{noremap = true, silent = true})
end

local function custom_attach(client)
	print("LSP started");
	-- require'completion'.on_attach(client)

	map('n','gD','<cmd>lua vim.lsp.buf.declaration()<CR>')
	map('n','gd','<cmd>lua vim.lsp.buf.definition()<CR>')
	map('n','K','<cmd>lua vim.lsp.buf.hover()<CR>')
	map("i", "<C-k>", "<cmd>lua vim.lsp.buf.signature_help()<CR>")
	map('n','gr','<cmd>lua vim.lsp.buf.references()<CR>')
	map('n','gs','<cmd>lua vim.lsp.buf.signature_help()<CR>')
	map('n','gi','<cmd>lua vim.lsp.buf.implementation()<CR>')
	map('n','gt','<cmd>lua vim.lsp.buf.type_definition()<CR>')
	map('n','gw','<cmd>lua vim.lsp.buf.document_symbol()<CR>')
	map('n','gW','<cmd>lua vim.lsp.buf.workspace_symbol()<CR>')
	map('n','<leader>laa','<cmd>lua vim.lsp.buf.code_action()<CR>')
	map('n','<leader>lah','<cmd>lua vim.lsp.buf.hover()<CR>')
	map('n','<leader>lar','<cmd>lua vim.lsp.buf.rename()<CR>')
	map('n','<leader>le','<cmd>lua vim.lsp.util.show_line_diagnostics()<CR>')
	map('n','<leader>=', '<cmd>lua vim.lsp.buf.formatting()<CR>')
	-- map('n','<leader>ai','<cmd>lua vim.lsp.buf.incoming_calls()<CR>')
	-- map('n','<leader>ao','<cmd>lua vim.lsp.buf.outgoing_calls()<CR>')
	map("n", "[d", "<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>")
	map("n", "]d", "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>")

	wk.register({
		l = {
			name = 'lsp',
			a = {
				name = 'action'
			},
		},
	}, {buffer = false})

end

require("lsp/nvim-lua").setup(custom_attach)
require'lspconfig'.zls.setup{
	on_attach = custom_attach
}

