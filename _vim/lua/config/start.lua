function _G.dump(...)
    local objects = vim.tbl_map(vim.inspect, {...})
    print(unpack(objects))
end

local lualine = require('lualine')
lualine.setup()
lualine.theme = 'gruvbox'

require'nvim-web-devicons'.setup({default=true})

require'colorizer'.setup()

require('config/telescope')
require("config/nvim-tree")
require("config/nvim-compe")

require'lspkind'.init()
require('lsp/config')

