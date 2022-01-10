local api = vim.api

local M = {}

function M.noremap(key, val)
	api.nvim_set_keymap('', key,  val, {noremap = true, silent = true})
end

function M.nnoremap(key, val)
	api.nvim_set_keymap('n', key,  val, {noremap = true, silent = true})
end

function M.vnoremap(key, val)
	api.nvim_set_keymap('v', key,  val, {noremap = true, silent = true})
end

function M.vnoremap(key, val)
	api.nvim_set_keymap('i', key,  val, {noremap = true, silent = true})
end

function M.tnoremap(key, val)
	api.nvim_set_keymap('t', key,  val, {noremap = true, silent = true})
end

return M
