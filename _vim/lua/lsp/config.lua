if vim.g.vscode then
	return
end

local wk = require("which-key")

local map = function(type, key, value)
	vim.api.nvim_buf_set_keymap(0,type,key,value,{noremap = true, silent = true})
end

local function on_attach(client)
	print("LSP started");
	-- require'completion'.on_attach(client)

	map('n','gD',	'<cmd>lua vim.lsp.buf.declaration()<CR>')
	map('n','gd',	'<cmd>lua vim.lsp.buf.definition()<CR>')
	map('n','K',	'<cmd>lua vim.lsp.buf.hover()<CR>')
	map("i",'<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>')
	map("n",'ga',	'<cmd>lua vim.lsp.buf.code_action()<CR>')
	map('n','gr',	'<cmd>lua vim.lsp.buf.references()<CR>')
	map('n','gs',	'<cmd>lua vim.lsp.buf.signature_help()<CR>')
	map('n','gi',	'<cmd>lua vim.lsp.buf.implementation()<CR>')
	map('n','gt',	'<cmd>lua vim.lsp.buf.type_definition()<CR>')
	map('n','gw',	'<cmd>lua vim.lsp.buf.document_symbol()<CR>')
	map('n','gW',	'<cmd>lua vim.lsp.buf.workspace_symbol()<CR>')
	map('n','<leader>laa',	'<cmd>lua vim.lsp.buf.code_action()<CR>')
	map('n','<leader>lah',	'<cmd>lua vim.lsp.buf.hover()<CR>')
	map('n','<leader>lar',	'<cmd>lua vim.lsp.buf.rename()<CR>')
	map('n','gl',	'<cmd>lua vim.diagnostic.open_float({scope="line"})<CR>')
	map('n','<leader>=',	'<cmd>lua vim.lsp.buf.formatting()<CR>')
	map("n", "[d",	'<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>')
	map("n", "]d",	'<cmd>lua vim.lsp.diagnostic.goto_next()<CR>')

	wk.register({
		l = {
			name = 'lsp',
			a = {
				name = 'action'
			},
		},
	}, {buffer = vim.api.nvim_get_current_buf(), prefix = "<localleader>"})

end

-- setup lsp installer
local lsp_installer = require("nvim-lsp-installer")
-- Provide settings first!
lsp_installer.settings{
	ui = {
		icons = {
			server_installed = "✓",
			server_pending = "➜",
			server_uninstalled = "✗",
		},
	},
}

local lsp_flags = {
  -- This is the default in Nvim 0.7+
  debounce_text_changes = 150,
}

lsp_installer.on_server_ready(function(server)
	local opts = {
		on_attach = on_attach,
		capabilities = require("cmp_nvim_lsp").update_capabilities(vim.lsp.protocol.make_client_capabilities()),
		flags = lsp_flags
	}

	if (server.name == "sumneko_lua") then
		opts = {
			on_attach = on_attach,
			capabilities = require("cmp_nvim_lsp").update_capabilities(vim.lsp.protocol.make_client_capabilities()),
			flags = lsp_flags,

    	cmd = {vim.fn.stdpath("data").."/lsp_servers/sumneko_lua/extension/server/bin/lua-language-server"},

    	settings = {
      	Lua = {
        	runtime = {
          	-- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
          	version = 'LuaJIT',
          	-- -- Setup your lua path
          	-- path = vim.split(package.path, ';'),
        	},
        	diagnostics = {
          	-- Get the language server to recognize the `vim` global
          	globals = {
            	-- vim
            	"vim",

            	-- Busted
            	"describe",
            	"it",
            	"before_each",
            	"after_each",
            	"teardown",
            	"pending",
            	"clear",

            	-- Colorbuddy
            	"Color",
            	"c",
            	"Group",
            	"g",
            	"s",

            	-- Custom
            	"RELOAD",
          	},
        	},
        	workspace = {
          	-- Make the server aware of Neovim runtime files
          	library = vim.api.nvim_get_runtime_file("", true),

        	},
      	},
    	},
		}
	end
	server:setup(opts)
end)


-- diagnostics
vim.diagnostic.config {
	virtual_text = false,
	underline = true,
	float = {
		source = "always",
	},
	severity_sort = true,
	--[[ virtual_text = {
	  prefix = "»",
	  spacing = 4,
	}, ]]
	signs = true,
	update_in_insert = false,
}
