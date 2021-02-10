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
