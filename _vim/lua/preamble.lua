vim.defer_fn(function()
  pcall(require, "impatient")
end, 0)

local cmd	= vim.cmd				-- execute Vim commands
local exec	= vim.api.nvim_exec		-- execute Vimscript
local fn	= vim.fn				-- call Vim functions
local g		= vim.g					-- global variables
local opt	= vim.opt				-- global/buffer/windows-scoped options
local api	= vim.api
local u		= require("util")

u.nnoremap('<SPACE>', '<Nop>')
g.mapleader      = " "    -- Leader key is Space
g.maplocalleader = ","    -- Local leader is comma
