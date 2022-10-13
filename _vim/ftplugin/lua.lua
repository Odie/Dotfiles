local wk = require("which-key")

wk.register({
    [','] = {function ()
        vim.api.nvim_command(":write")
        vim.api.nvim_command(":source %")
    end,
    'Save and Eval file'},
}, { prefix = "<localleader>", buffer = vim.api.nvim_get_current_buf()})
