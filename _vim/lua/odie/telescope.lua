local find_command = {'rg','--follow','--ignore','--hidden','--files', '--glob', '!.git'}

local function find_files()
  require('telescope.builtin').find_files({find_command = find_command})
end

return {
  find_files = find_files,
  find_command = find_command
}

