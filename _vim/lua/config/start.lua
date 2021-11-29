function _G.dump(...)
    local objects = vim.tbl_map(vim.inspect, {...})
    print(unpack(objects))
end

-- local lualine = require('lualine')
-- lualine.setup()
-- lualine.theme = 'gruvbox'

-- require('config/telescope')

-- require("config/nvim-compe")

-- require'lspkind'.init()
-- require('lsp/config')

--require('config/which-key')
