function _G.dump(...)
    local objects = vim.tbl_map(vim.inspect, {...})
    print(unpack(objects))
end

local lualine = require('lualine')
lualine.status()
lualine.theme = 'gruvbox'

require'nvim-web-devicons'.setup({default=true})

require'lspkind'.init()

require'colorizer'.setup()

----------------------------------------------
-- Enable <c-j> and <c-k> in telescope
local actions = require('telescope.actions')

require('telescope').setup{
  defaults = {
    mappings = {
      i = {
        ["<C-j>"] = actions.move_selection_next,
        ["<C-k>"] = actions.move_selection_previous
      }
    }
  }
}

require('lsp/config')

require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  highlight = {
    enable = true,              -- false will disable the whole extension
    disable = { "c", "rust" },  -- list of language that will be disabled
  },
}

require('tigam')
