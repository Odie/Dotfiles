-- Automatically generated packer.nvim plugin loader code

if vim.api.nvim_call_function('has', {'nvim-0.5'}) ~= 1 then
  vim.api.nvim_command('echohl WarningMsg | echom "Invalid Neovim version for packer.nvim! | echohl None"')
  return
end

vim.api.nvim_command('packadd packer.nvim')

local no_errors, error_msg = pcall(function()

  local time
  local profile_info
  local should_profile = false
  if should_profile then
    local hrtime = vim.loop.hrtime
    profile_info = {}
    time = function(chunk, start)
      if start then
        profile_info[chunk] = hrtime()
      else
        profile_info[chunk] = (hrtime() - profile_info[chunk]) / 1e6
      end
    end
  else
    time = function(chunk, start) end
  end
  
local function save_profiles(threshold)
  local sorted_times = {}
  for chunk_name, time_taken in pairs(profile_info) do
    sorted_times[#sorted_times + 1] = {chunk_name, time_taken}
  end
  table.sort(sorted_times, function(a, b) return a[2] > b[2] end)
  local results = {}
  for i, elem in ipairs(sorted_times) do
    if not threshold or threshold and elem[2] > threshold then
      results[i] = elem[1] .. ' took ' .. elem[2] .. 'ms'
    end
  end

  _G._packer = _G._packer or {}
  _G._packer.profile_output = results
end

time([[Luarocks path setup]], true)
local package_path_str = "/Users/odie/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?.lua;/Users/odie/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?/init.lua;/Users/odie/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?.lua;/Users/odie/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/Users/odie/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

time([[Luarocks path setup]], false)
time([[try_loadstring definition]], true)
local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s), name, _G.packer_plugins[name])
  if not success then
    vim.schedule(function()
      vim.api.nvim_notify('packer.nvim: Error running ' .. component .. ' for ' .. name .. ': ' .. result, vim.log.levels.ERROR, {})
    end)
  end
  return result
end

time([[try_loadstring definition]], false)
time([[Defining packer_plugins]], true)
_G.packer_plugins = {
  VimAutoMakeDirectory = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/VimAutoMakeDirectory",
    url = "https://github.com/jordwalke/VimAutoMakeDirectory"
  },
  ["a.vim"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/a.vim",
    url = "https://github.com/nacitar/a.vim"
  },
  aniseed = {
    loaded = false,
    needs_bufread = true,
    only_cond = false,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/opt/aniseed",
    url = "https://github.com/Olical/aniseed"
  },
  ["barbar.nvim"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/barbar.nvim",
    url = "https://github.com/romgrk/barbar.nvim"
  },
  ["base16-vim"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/base16-vim",
    url = "https://github.com/chriskempson/base16-vim"
  },
  ["bufferize.vim"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/bufferize.vim",
    url = "https://github.com/AndrewRadev/bufferize.vim"
  },
  ["cmp-buffer"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/cmp-buffer",
    url = "https://github.com/hrsh7th/cmp-buffer"
  },
  ["cmp-cmdline"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/cmp-cmdline",
    url = "https://github.com/hrsh7th/cmp-cmdline"
  },
  ["cmp-nvim-lsp"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/cmp-nvim-lsp",
    url = "https://github.com/hrsh7th/cmp-nvim-lsp"
  },
  ["cmp-path"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/cmp-path",
    url = "https://github.com/hrsh7th/cmp-path"
  },
  ["cmp-vsnip"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/cmp-vsnip",
    url = "https://github.com/hrsh7th/cmp-vsnip"
  },
  conjure = {
    config = { "\27LJ\2\n=\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0&let g:conjure#log#botright = true\bcmd\0" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/opt/conjure",
    url = "https://github.com/Olical/conjure"
  },
  ["dash.vim"] = {
    config = { "\27LJ\2\nT\0\0\3\0\3\0\0056\0\0\0009\0\1\0'\2\2\0B\0\2\1K\0\1\0005\t\t\t\tnmap <silent> <leader>d <Plug>DashSearch\n\t\t\t\bcmd\bvim\0" },
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/dash.vim",
    url = "https://github.com/rizzatti/dash.vim"
  },
  detectindent = {
    commands = { "DetectIndent" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/opt/detectindent",
    url = "https://github.com/ciaranm/detectindent"
  },
  ["emmet-vim"] = {
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/opt/emmet-vim",
    url = "https://github.com/mattn/emmet-vim"
  },
  ["fennel.vim"] = {
    loaded = false,
    needs_bufread = true,
    only_cond = false,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/opt/fennel.vim",
    url = "https://github.com/bakpakin/fennel.vim"
  },
  gitabra = {
    commands = { "Gitabra" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/opt/gitabra",
    url = "/Users/odie/dev/vim/gitabra"
  },
  ["goyo.vim"] = {
    commands = { "Goyo" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/opt/goyo.vim",
    url = "https://github.com/junegunn/goyo.vim"
  },
  gruvbox = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/gruvbox",
    url = "https://github.com/morhetz/gruvbox"
  },
  ["gruvbox-material"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/gruvbox-material",
    url = "https://github.com/sainnhe/gruvbox-material"
  },
  ["listmaps.vim"] = {
    commands = { "Listmaps" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/opt/listmaps.vim",
    url = "https://github.com/vim-scripts/listmaps.vim"
  },
  ["lspkind-nvim"] = {
    config = { "\27LJ\2\nK\0\0\3\0\4\0\t6\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\0016\0\0\0'\2\3\0B\0\2\1K\0\1\0\15lsp/config\tinit\flspkind\frequire\0" },
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/lspkind-nvim",
    url = "https://github.com/onsails/lspkind-nvim"
  },
  ["lualine.nvim"] = {
    config = { "\27LJ\2\nK\0\0\3\0\5\0\b6\0\0\0'\2\1\0B\0\2\0029\1\2\0B\1\1\1'\1\4\0=\1\3\0K\0\1\0\fgruvbox\ntheme\nsetup\flualine\frequire\0" },
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/lualine.nvim",
    url = "https://github.com/nvim-lualine/lualine.nvim"
  },
  neomake = {
    config = { "\27LJ\2\nK\0\0\3\0\3\0\0056\0\0\0009\0\1\0'\2\2\0B\0\2\1K\0\1\0,\t\t\t\tautocmd! BufWritePost * Neomake\n\t\t\t\bcmd\bvim\0" },
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/neomake",
    url = "https://github.com/neomake/neomake"
  },
  ["nimrod.vim"] = {
    loaded = false,
    needs_bufread = true,
    only_cond = false,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/opt/nimrod.vim",
    url = "https://github.com/zah/nimrod.vim"
  },
  ["nvim-cmp"] = {
    config = { "\27LJ\2\n;\0\1\4\0\4\0\0066\1\0\0009\1\1\0019\1\2\0019\3\3\0B\1\2\1K\0\1\0\tbody\20vsnip#anonymous\afn\bvimá\b\1\0\v\0<\0y6\0\0\0009\0\1\0'\1\3\0=\1\2\0006\0\4\0'\2\5\0B\0\2\0029\1\6\0005\3\n\0005\4\b\0003\5\a\0=\5\t\4=\4\v\0035\4\19\0009\5\f\0009\a\f\0009\a\r\a5\t\16\0009\n\14\0009\n\15\n=\n\17\tB\a\2\0025\b\18\0B\5\3\2=\5\20\0049\5\f\0009\a\f\0009\a\21\a5\t\22\0009\n\14\0009\n\15\n=\n\17\tB\a\2\0025\b\23\0B\5\3\2=\5\24\0049\5\f\0009\a\f\0009\a\25\a)\t¸ˇB\a\2\0025\b\26\0B\5\3\2=\5\27\0049\5\f\0009\a\f\0009\a\25\a)\t\4\0B\a\2\0025\b\28\0B\5\3\2=\5\29\0049\5\f\0009\5\30\0055\a!\0009\b\31\0009\b \b=\b\17\aB\5\2\2=\5\"\4=\4\f\0039\4#\0009\4$\0044\6\3\0005\a%\0>\a\1\0065\a&\0>\a\2\0064\a\3\0005\b'\0>\b\1\aB\4\3\2=\4$\3B\1\2\0019\1\6\0009\1(\1'\3)\0005\4+\0004\5\3\0005\6*\0>\6\1\5=\5$\4B\1\3\0019\1\6\0009\1(\1'\3,\0005\4/\0009\5#\0009\5$\0054\a\3\0005\b-\0>\b\1\a4\b\3\0005\t.\0>\t\1\bB\5\3\2=\5$\4B\1\3\0016\1\4\0'\0030\0B\1\2\0029\2\6\0005\0045\0005\0053\0009\0061\0015\b2\0B\6\2\2=\0064\5=\0056\4B\2\2\0016\2\4\0'\0047\0B\2\2\0029\0028\0026\4\0\0009\0049\0049\4:\0049\4;\4B\4\1\0A\2\0\2K\0\1\0\29make_client_capabilities\rprotocol\blsp\24update_capabilities\17cmp_nvim_lsp\15formatting\1\0\0\vformat\1\0\0\1\0\2\14with_text\1\rmaxwidth\0032\15cmp_format\flspkind\1\0\0\1\0\1\tname\fcmdline\1\0\1\tname\tpath\6:\1\0\0\1\0\1\tname\vbuffer\6/\fcmdline\1\0\1\tname\vbuffer\1\0\1\tname\nvsnip\1\0\1\tname\rnvim_lsp\fsources\vconfig\t<CR>\1\0\1\vselect\2\fReplace\20ConfirmBehavior\fconfirm\n<C-f>\1\3\0\0\6i\6c\n<C-b>\1\3\0\0\6i\6c\16scroll_docs\n<C-k>\1\3\0\0\6i\6c\1\0\0\21select_prev_item\n<C-j>\1\0\0\1\3\0\0\6i\6c\rbehavior\1\0\0\vInsert\19SelectBehavior\21select_next_item\fmapping\fsnippet\1\0\0\vexpand\1\0\0\0\nsetup\bcmp\frequire\26menu,menuone,noselect\16completeopt\6o\bvim\0" },
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/nvim-cmp",
    url = "https://github.com/hrsh7th/nvim-cmp"
  },
  ["nvim-colorizer.lua"] = {
    config = { "\27LJ\2\n7\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\14colorizer\frequire\0" },
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/nvim-colorizer.lua",
    url = "https://github.com/norcalli/nvim-colorizer.lua"
  },
  ["nvim-lspconfig"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/nvim-lspconfig",
    url = "https://github.com/neovim/nvim-lspconfig"
  },
  ["nvim-tree.lua"] = {
    commands = { "NvimTreeToggle" },
    config = { "\27LJ\2\nñ\5\0\0\6\0\29\0%6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\3\0004\3\0\0=\3\4\0025\3\5\0=\3\6\0025\3\a\0005\4\b\0=\4\t\3=\3\n\0025\3\v\0004\4\0\0=\4\f\3=\3\r\0025\3\14\0004\4\0\0=\4\15\3=\3\16\0025\3\17\0004\4\0\0=\4\18\3=\3\19\0025\3\20\0005\4\21\0004\5\0\0=\5\22\4=\4\23\3=\3\24\2B\0\2\0016\0\25\0009\0\26\0005\1\28\0=\1\27\0K\0\1\0\1\0\3\nfiles\3\1\ffolders\3\1\bgit\3\1\25nvim_tree_show_icons\6g\bvim\tview\rmappings\tlist\1\0\1\16custom_only\1\1\0\5\16auto_resize\1\tside\tleft\21hide_root_folder\1\vheight\3\30\nwidth\3\30\ffilters\vcustom\1\0\1\rdotfiles\1\16system_open\targs\1\0\0\24update_focused_file\16ignore_list\1\0\2\venable\1\15update_cwd\1\16diagnostics\nicons\1\0\4\nerror\bÔÅó\thint\bÔÅ™\fwarning\bÔÅ±\tinfo\bÔÅö\1\0\1\venable\1\22update_to_buf_dir\1\0\2\14auto_open\2\venable\2\23ignore_ft_on_setup\1\0\a\17hijack_netrw\2\18disable_netrw\2\15update_cwd\1\18hijack_cursor\1\16open_on_tab\1\15auto_close\1\18open_on_setup\1\nsetup\14nvim-tree\frequire\0" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/opt/nvim-tree.lua",
    url = "https://github.com/kyazdani42/nvim-tree.lua"
  },
  ["nvim-treesitter"] = {
    config = { "\27LJ\2\n†\1\0\0\5\0\b\0\v6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\3\0005\3\4\0005\4\5\0=\4\6\3=\3\a\2B\0\2\1K\0\1\0\14highlight\fdisable\1\3\0\0\6c\trust\1\0\1\venable\2\1\0\1\21ensure_installed\15maintained\nsetup\28nvim-treesitter.configs\frequire\0" },
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/nvim-treesitter",
    url = "https://github.com/nvim-treesitter/nvim-treesitter"
  },
  ["nvim-web-devicons"] = {
    config = { "\27LJ\2\nO\0\0\3\0\4\0\a6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\3\0B\0\2\1K\0\1\0\1\0\1\fdefault\2\nsetup\22nvim-web-devicons\frequire\0" },
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/nvim-web-devicons",
    url = "https://github.com/kyazdani42/nvim-web-devicons"
  },
  ["packer.nvim"] = {
    loaded = false,
    needs_bufread = false,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/opt/packer.nvim",
    url = "https://github.com/wbthomason/packer.nvim"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/plenary.nvim",
    url = "https://github.com/nvim-lua/plenary.nvim"
  },
  ["popup.nvim"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/popup.nvim",
    url = "https://github.com/nvim-lua/popup.nvim"
  },
  ["python-syntax"] = {
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/opt/python-syntax",
    url = "https://github.com/hdima/python-syntax"
  },
  rainbow = {
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/opt/rainbow",
    url = "https://github.com/luochen1990/rainbow"
  },
  ["swift.vim"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/swift.vim",
    url = "https://github.com/keith/swift.vim"
  },
  tabular = {
    after_files = { "/Users/odie/.local/share/nvim/site/pack/packer/opt/tabular/after/plugin/TabularMaps.vim" },
    commands = { "Tabularize" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/opt/tabular",
    url = "https://github.com/godlygeek/tabular"
  },
  tcomment_vim = {
    config = { "\27LJ\2\n6\0\0\2\0\4\0\0056\0\0\0009\0\1\0'\1\3\0=\1\2\0K\0\1\0\5\24tcomment_mapleader2\6g\bvim\0" },
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/tcomment_vim",
    url = "https://github.com/tomtom/tcomment_vim"
  },
  ["telescope.nvim"] = {
    config = { "\27LJ\2\n®\t\0\0\b\0/\0K6\0\0\0'\2\1\0B\0\2\0026\1\0\0'\3\2\0B\1\2\0029\1\3\0015\3*\0005\4\5\0005\5\4\0=\5\6\0045\5\b\0005\6\a\0=\6\t\0055\6\n\0=\6\v\5=\5\f\0046\5\0\0'\a\r\0B\5\2\0029\5\14\5=\5\15\0044\5\0\0=\5\16\0046\5\0\0'\a\r\0B\5\2\0029\5\17\5=\5\18\0044\5\0\0=\5\19\0045\5\20\0=\5\21\0044\5\0\0=\5\22\0045\5\23\0=\5\24\0046\5\0\0'\a\25\0B\5\2\0029\5\26\0059\5\27\5=\5\28\0046\5\0\0'\a\25\0B\5\2\0029\5\29\0059\5\27\5=\5\30\0046\5\0\0'\a\25\0B\5\2\0029\5\31\0059\5\27\5=\5 \0046\5\0\0'\a\25\0B\5\2\0029\5!\5=\5!\0045\5'\0005\6#\0009\a\"\0=\a$\0069\a%\0=\a&\6=\6(\5=\5)\4=\4+\3B\1\2\0016\1,\0009\1-\1'\3.\0B\1\2\1K\0\1\0/highlight TelescopeSelection guibg=#3C3836\bcmd\bvim\rdefaults\1\0\0\rmappings\6i\1\0\0\n<C-k>\28move_selection_previous\n<C-j>\1\0\0\24move_selection_next\27buffer_previewer_maker\21qflist_previewer\22vim_buffer_qflist\19grep_previewer\23vim_buffer_vimgrep\19file_previewer\bnew\19vim_buffer_cat\25telescope.previewers\fset_env\1\0\1\14COLORTERM\14truecolor\17path_display\16borderchars\1\t\0\0\b‚îÄ\b‚îÇ\b‚îÄ\b‚îÇ\b‚ï≠\b‚ïÆ\b‚ïØ\b‚ï∞\vborder\19generic_sorter\29get_generic_fuzzy_sorter\25file_ignore_patterns\16file_sorter\19get_fuzzy_file\22telescope.sorters\18layout_config\rvertical\1\0\1\vmirror\1\15horizontal\1\0\0\1\0\1\vmirror\1\22vimgrep_arguments\1\0\n\rwinblend\3\0\20layout_strategy\15horizontal\21sorting_strategy\15descending\23selection_strategy\nreset\17initial_mode\vinsert\17entry_prefix\a  \20selection_caret\a> \18prompt_prefix\a> \ruse_less\2\19color_devicons\2\1\b\0\0\arg\18--color=never\17--no-heading\20--with-filename\18--line-number\r--column\17--smart-case\nsetup\14telescope\22telescope.actions\frequire\0" },
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/telescope.nvim",
    url = "https://github.com/nvim-telescope/telescope.nvim"
  },
  ["vim-bbye"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/vim-bbye",
    url = "https://github.com/moll/vim-bbye"
  },
  ["vim-blade"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/vim-blade",
    url = "https://github.com/xsbeats/vim-blade"
  },
  ["vim-colorschemes"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/vim-colorschemes",
    url = "https://github.com/flazz/vim-colorschemes"
  },
  ["vim-easymotion"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/vim-easymotion",
    url = "https://github.com/Lokaltog/vim-easymotion"
  },
  ["vim-esearch"] = {
    config = { "\27LJ\2\n°\f\0\0\5\0(\0M6\0\0\0009\0\1\0005\1\3\0004\2#\0005\3\4\0>\3\1\0025\3\5\0>\3\2\0025\3\6\0>\3\3\0025\3\a\0>\3\4\0025\3\b\0>\3\5\0025\3\t\0>\3\6\0025\3\n\0>\3\a\0025\3\v\0>\3\b\0025\3\f\0>\3\t\0025\3\r\0>\3\n\0025\3\14\0>\3\v\0025\3\15\0>\3\f\0025\3\16\0>\3\r\0025\3\17\0>\3\14\0025\3\18\0>\3\15\0025\3\19\0>\3\16\0025\3\20\0>\3\17\0025\3\21\0>\3\18\0025\3\22\0>\3\19\0025\3\23\0005\4\24\0>\4\4\3>\3\20\0025\3\25\0>\3\21\0025\3\26\0>\3\22\0025\3\27\0>\3\23\0025\3\28\0>\3\24\0025\3\29\0>\3\25\0025\3\30\0>\3\26\0025\3\31\0>\3\27\0025\3 \0>\3\28\0025\3!\0>\3\29\0025\3\"\0>\3\30\0025\3#\0>\3\31\0025\3$\0>\3 \0025\3%\0>\3!\0025\3&\0>\3\"\2=\2'\1=\1\2\0K\0\1\0\fwin_map\1\4\0\0\6n\azM\23<plug>(esearch-zM)\1\4\0\0\6n\azc\23<plug>(esearch-zc)\1\4\0\0\6n\aza\23<plug>(esearch-za)\1\4\0\0\6n\a@:\23<plug>(esearch-@:)\1\4\0\0\6n\6.\22<plug>(esearch-.)\1\4\0\0\6x\6s\22<plug>(esearch-c)\1\4\0\0\anx\6D\22<plug>(esearch-D)\1\4\0\0\anx\6C\22<plug>(esearch-C)\1\4\0\0\6n\acc\23<plug>(esearch-cc)\1\4\0\0\anx\6c\22<plug>(esearch-c)\1\4\0\0\6n\add\23<plug>(esearch-dd)\1\4\0\0\anx\6d\22<plug>(esearch-d)\1\4\0\0\6x\6x\22<plug>(esearch-d)\1\4\0\0\6n\6I\22<plug>(esearch-I)\1\0\1\vnowait\3\1\1\4\0\0\aic\t<cr>\23<plug>(esearch-cr)\1\4\0\0\aov\aam$<plug>(textobj-esearch-match-a)\1\4\0\0\aov\aim$<plug>(textobj-esearch-match-i)\1\4\0\0\6 \6((<plug>(esearch-win-jump:dirname:up)\1\4\0\0\6 \6)*<plug>(esearch-win-jump:dirname:down)\1\4\0\0\6 \6{)<plug>(esearch-win-jump:filename:up)\1\4\0\0\6 \6}+<plug>(esearch-win-jump:filename:down)\1\4\0\0\6 \6K&<plug>(esearch-win-jump:entry:up)\1\4\0\0\6 \6J(<plug>(esearch-win-jump:entry:down)\1\4\0\0\6n\n<esc>&<plug>(esearch-win-preview:close)\1\4\0\0\6n\6P)100<plug>(esearch-win-preview:enter)\1\4\0\0\6n\6p <plug>(esearch-win-preview)\1\4\0\0\6n\t<cr>\29<plug>(esearch-win-open)\1\4\0\0\6n\6S*<plug>(esearch-win-vsplit:reuse:stay)\1\4\0\0\6n\6s\31<plug>(esearch-win-vsplit)\1\4\0\0\6n\6O)<plug>(esearch-win-split:reuse:stay)\1\4\0\0\6n\6o\30<plug>(esearch-win-split)\1\4\0\0\6n\6T%<plug>(esearch-win-tabopen:stay)\1\4\0\0\6n\6t <plug>(esearch-win-tabopen)\1\4\0\0\6n\6R\31<plug>(esearch-win-reload)\1\0\1\21default_mappings\3\0\fesearch\6g\bvim\0" },
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/vim-esearch",
    url = "https://github.com/eugen0329/vim-esearch"
  },
  ["vim-eunuch"] = {
    commands = { "Remove", "Unlink", "Move", "Rename", "Chmod", "Mkdir", "Find", "Locate", "Wall", "SudoEdit", "SudoWrite" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/opt/vim-eunuch",
    url = "https://github.com/tpope/vim-eunuch"
  },
  ["vim-flake8"] = {
    loaded = false,
    needs_bufread = true,
    only_cond = false,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/opt/vim-flake8",
    url = "https://github.com/nvie/vim-flake8"
  },
  ["vim-fugitive"] = {
    commands = { "Git" },
    loaded = false,
    needs_bufread = true,
    only_cond = false,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/opt/vim-fugitive",
    url = "https://github.com/tpope/vim-fugitive"
  },
  ["vim-indent-guides"] = {
    config = { '\27LJ\2\n?\0\0\2\0\3\0\0056\0\0\0009\0\1\0)\1\0\0=\1\2\0K\0\1\0"indent_guides_default_mapping\6g\bvim\0' },
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/vim-indent-guides",
    url = "https://github.com/nathanaelkane/vim-indent-guides"
  },
  ["vim-less"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/vim-less",
    url = "https://github.com/groenewege/vim-less"
  },
  ["vim-livedown"] = {
    config = { "\27LJ\2\nn\0\0\2\0\5\0\r6\0\0\0009\0\1\0)\1\1\0=\1\2\0006\0\0\0009\0\1\0)\1\1\0=\1\3\0006\0\0\0009\0\1\0)\1–\a=\1\4\0K\0\1\0\18livedown_port\18livedown_open\21livedown_autorun\6g\bvim\0" },
    loaded = false,
    needs_bufread = true,
    only_cond = false,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/opt/vim-livedown",
    url = "https://github.com/shime/vim-livedown"
  },
  ["vim-markdown"] = {
    loaded = false,
    needs_bufread = true,
    only_cond = false,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/opt/vim-markdown",
    url = "https://github.com/tpope/vim-markdown"
  },
  ["vim-matchup"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/vim-matchup",
    url = "https://github.com/andymass/vim-matchup"
  },
  ["vim-misc"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/vim-misc",
    url = "https://github.com/xolox/vim-misc"
  },
  ["vim-multiple-cursors"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/vim-multiple-cursors",
    url = "https://github.com/terryma/vim-multiple-cursors"
  },
  ["vim-repeat"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/vim-repeat",
    url = "https://github.com/tpope/vim-repeat"
  },
  ["vim-rooter"] = {
    config = { "\27LJ\2\nZ\0\0\2\0\4\0\t6\0\0\0009\0\1\0)\1\1\0=\1\2\0006\0\0\0009\0\1\0)\1\1\0=\1\3\0K\0\1\0\25rooter_resolve_links\24rooter_silent_chdir\6g\bvim\0" },
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/vim-rooter",
    url = "https://github.com/airblade/vim-rooter"
  },
  ["vim-sexp"] = {
    config = { '\27LJ\2\nû\f\0\0\2\0\4\0\0056\0\0\0009\0\1\0005\1\3\0=\1\2\0K\0\1\0\1\0006\23sexp_inner_element\aie!sexp_curly_head_wrap_element\5\23sexp_outer_element\aae\22sexp_inner_string\ais\28sexp_swap_list_backward\n<M-k>\22sexp_outer_string\aas\20sexp_raise_list\5\24sexp_inner_top_list\aiF\21sexp_splice_list\5\24sexp_outer_top_list\aaF\20sexp_inner_list\aif!sexp_curly_tail_wrap_element\5\20sexp_outer_list\aaf"sexp_square_tail_wrap_element\5"sexp_square_head_wrap_element\5!sexp_round_tail_wrap_element\5!sexp_round_head_wrap_element\5\30sexp_curly_tail_wrap_list\5\30sexp_curly_head_wrap_list\5\31sexp_square_tail_wrap_list\5\31sexp_square_head_wrap_list\5\30sexp_round_tail_wrap_list\5\30sexp_round_head_wrap_list\5\19sexp_convolute\5\20sexp_indent_top\a=-\29sexp_insert_at_list_tail\5\16sexp_indent\a==\23sexp_raise_element\5\29sexp_select_next_element\a]e\29sexp_insert_at_list_head\5\29sexp_select_prev_element\a[e\27sexp_swap_list_forward\n<M-j>"sexp_move_to_next_top_element\a]]\31sexp_swap_element_backward\n<M-h>"sexp_move_to_prev_top_element\a[[\30sexp_swap_element_forward\n<M-l> sexp_flow_to_next_leaf_tail\f<M-S-e>\27sexp_emit_head_element\f<M-S-j> sexp_flow_to_prev_leaf_tail\f<M-S-g>\27sexp_emit_tail_element\f<M-S-k> sexp_flow_to_next_leaf_head\f<M-S-w>\30sexp_capture_prev_element\f<M-S-h> sexp_flow_to_prev_leaf_head\f<M-S-b>\30sexp_capture_next_element\f<M-S-l>\28sexp_flow_to_next_close\n<M-}>\27sexp_flow_to_prev_open\n<M-{>\27sexp_flow_to_next_open\n<M-]>\28sexp_flow_to_prev_close\n<M-[>#sexp_move_to_next_element_tail\n<M-e>#sexp_move_to_prev_element_tail\vg<M-e>#sexp_move_to_next_element_head\n<M-w>#sexp_move_to_prev_element_head\n<M-b>\30sexp_move_to_next_bracket\6)\30sexp_move_to_prev_bracket\6(\18sexp_mappings\6g\bvim\0' },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/opt/vim-sexp",
    url = "https://github.com/guns/vim-sexp"
  },
  ["vim-sexp-mappings-for-regular-people"] = {
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/opt/vim-sexp-mappings-for-regular-people",
    url = "https://github.com/tpope/vim-sexp-mappings-for-regular-people"
  },
  ["vim-startify"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/vim-startify",
    url = "https://github.com/mhinz/vim-startify"
  },
  ["vim-startuptime"] = {
    commands = { "StartupTime" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/opt/vim-startuptime",
    url = "https://github.com/dstein64/vim-startuptime"
  },
  ["vim-surround"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/vim-surround",
    url = "https://github.com/tpope/vim-surround"
  },
  ["vim-unimpaired"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/vim-unimpaired",
    url = "https://github.com/tpope/vim-unimpaired"
  },
  ["vim-vsnip"] = {
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/vim-vsnip",
    url = "https://github.com/hrsh7th/vim-vsnip"
  },
  ["which-key.nvim"] = {
    config = { "\27LJ\2\n0\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\21config/which-key\frequire\0" },
    loaded = true,
    path = "/Users/odie/.local/share/nvim/site/pack/packer/start/which-key.nvim",
    url = "https://github.com/folke/which-key.nvim"
  }
}

time([[Defining packer_plugins]], false)
-- Setup for: detectindent
time([[Setup for detectindent]], true)
try_loadstring("\27LJ\2\nÁ\1\0\0\3\0\6\0\r6\0\0\0009\0\1\0)\1\4\0=\1\2\0006\0\0\0009\0\1\0)\1\1\0=\1\3\0006\0\0\0009\0\4\0'\2\5\0B\0\2\1K\0\1\0g\t\t\t\taugroup AutoDetectIndent\n\t\t\t\t\tau!\n\t\t\t\t\tautocmd BufReadPost * :DetectIndent\n\t\t\t\taugroup end\n\t\t\t\bcmd%detectindent_preferred_expandtab\"detectindent_preferred_indent\6g\bvim\0", "setup", "detectindent")
time([[Setup for detectindent]], false)
-- Setup for: gitabra
time([[Setup for gitabra]], true)
try_loadstring("\27LJ\2\n-\0\0\2\0\3\0\0056\0\0\0009\0\1\0)\1\1\0=\1\2\0K\0\1\0\16gitabra_dev\6g\bvim\0", "setup", "gitabra")
time([[Setup for gitabra]], false)
-- Config for: lspkind-nvim
time([[Config for lspkind-nvim]], true)
try_loadstring("\27LJ\2\nK\0\0\3\0\4\0\t6\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\0016\0\0\0'\2\3\0B\0\2\1K\0\1\0\15lsp/config\tinit\flspkind\frequire\0", "config", "lspkind-nvim")
time([[Config for lspkind-nvim]], false)
-- Config for: nvim-colorizer.lua
time([[Config for nvim-colorizer.lua]], true)
try_loadstring("\27LJ\2\n7\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\14colorizer\frequire\0", "config", "nvim-colorizer.lua")
time([[Config for nvim-colorizer.lua]], false)
-- Config for: lualine.nvim
time([[Config for lualine.nvim]], true)
try_loadstring("\27LJ\2\nK\0\0\3\0\5\0\b6\0\0\0'\2\1\0B\0\2\0029\1\2\0B\1\1\1'\1\4\0=\1\3\0K\0\1\0\fgruvbox\ntheme\nsetup\flualine\frequire\0", "config", "lualine.nvim")
time([[Config for lualine.nvim]], false)
-- Config for: which-key.nvim
time([[Config for which-key.nvim]], true)
try_loadstring("\27LJ\2\n0\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\21config/which-key\frequire\0", "config", "which-key.nvim")
time([[Config for which-key.nvim]], false)
-- Config for: vim-esearch
time([[Config for vim-esearch]], true)
try_loadstring("\27LJ\2\n°\f\0\0\5\0(\0M6\0\0\0009\0\1\0005\1\3\0004\2#\0005\3\4\0>\3\1\0025\3\5\0>\3\2\0025\3\6\0>\3\3\0025\3\a\0>\3\4\0025\3\b\0>\3\5\0025\3\t\0>\3\6\0025\3\n\0>\3\a\0025\3\v\0>\3\b\0025\3\f\0>\3\t\0025\3\r\0>\3\n\0025\3\14\0>\3\v\0025\3\15\0>\3\f\0025\3\16\0>\3\r\0025\3\17\0>\3\14\0025\3\18\0>\3\15\0025\3\19\0>\3\16\0025\3\20\0>\3\17\0025\3\21\0>\3\18\0025\3\22\0>\3\19\0025\3\23\0005\4\24\0>\4\4\3>\3\20\0025\3\25\0>\3\21\0025\3\26\0>\3\22\0025\3\27\0>\3\23\0025\3\28\0>\3\24\0025\3\29\0>\3\25\0025\3\30\0>\3\26\0025\3\31\0>\3\27\0025\3 \0>\3\28\0025\3!\0>\3\29\0025\3\"\0>\3\30\0025\3#\0>\3\31\0025\3$\0>\3 \0025\3%\0>\3!\0025\3&\0>\3\"\2=\2'\1=\1\2\0K\0\1\0\fwin_map\1\4\0\0\6n\azM\23<plug>(esearch-zM)\1\4\0\0\6n\azc\23<plug>(esearch-zc)\1\4\0\0\6n\aza\23<plug>(esearch-za)\1\4\0\0\6n\a@:\23<plug>(esearch-@:)\1\4\0\0\6n\6.\22<plug>(esearch-.)\1\4\0\0\6x\6s\22<plug>(esearch-c)\1\4\0\0\anx\6D\22<plug>(esearch-D)\1\4\0\0\anx\6C\22<plug>(esearch-C)\1\4\0\0\6n\acc\23<plug>(esearch-cc)\1\4\0\0\anx\6c\22<plug>(esearch-c)\1\4\0\0\6n\add\23<plug>(esearch-dd)\1\4\0\0\anx\6d\22<plug>(esearch-d)\1\4\0\0\6x\6x\22<plug>(esearch-d)\1\4\0\0\6n\6I\22<plug>(esearch-I)\1\0\1\vnowait\3\1\1\4\0\0\aic\t<cr>\23<plug>(esearch-cr)\1\4\0\0\aov\aam$<plug>(textobj-esearch-match-a)\1\4\0\0\aov\aim$<plug>(textobj-esearch-match-i)\1\4\0\0\6 \6((<plug>(esearch-win-jump:dirname:up)\1\4\0\0\6 \6)*<plug>(esearch-win-jump:dirname:down)\1\4\0\0\6 \6{)<plug>(esearch-win-jump:filename:up)\1\4\0\0\6 \6}+<plug>(esearch-win-jump:filename:down)\1\4\0\0\6 \6K&<plug>(esearch-win-jump:entry:up)\1\4\0\0\6 \6J(<plug>(esearch-win-jump:entry:down)\1\4\0\0\6n\n<esc>&<plug>(esearch-win-preview:close)\1\4\0\0\6n\6P)100<plug>(esearch-win-preview:enter)\1\4\0\0\6n\6p <plug>(esearch-win-preview)\1\4\0\0\6n\t<cr>\29<plug>(esearch-win-open)\1\4\0\0\6n\6S*<plug>(esearch-win-vsplit:reuse:stay)\1\4\0\0\6n\6s\31<plug>(esearch-win-vsplit)\1\4\0\0\6n\6O)<plug>(esearch-win-split:reuse:stay)\1\4\0\0\6n\6o\30<plug>(esearch-win-split)\1\4\0\0\6n\6T%<plug>(esearch-win-tabopen:stay)\1\4\0\0\6n\6t <plug>(esearch-win-tabopen)\1\4\0\0\6n\6R\31<plug>(esearch-win-reload)\1\0\1\21default_mappings\3\0\fesearch\6g\bvim\0", "config", "vim-esearch")
time([[Config for vim-esearch]], false)
-- Config for: dash.vim
time([[Config for dash.vim]], true)
try_loadstring("\27LJ\2\nT\0\0\3\0\3\0\0056\0\0\0009\0\1\0'\2\2\0B\0\2\1K\0\1\0005\t\t\t\tnmap <silent> <leader>d <Plug>DashSearch\n\t\t\t\bcmd\bvim\0", "config", "dash.vim")
time([[Config for dash.vim]], false)
-- Config for: neomake
time([[Config for neomake]], true)
try_loadstring("\27LJ\2\nK\0\0\3\0\3\0\0056\0\0\0009\0\1\0'\2\2\0B\0\2\1K\0\1\0,\t\t\t\tautocmd! BufWritePost * Neomake\n\t\t\t\bcmd\bvim\0", "config", "neomake")
time([[Config for neomake]], false)
-- Config for: vim-rooter
time([[Config for vim-rooter]], true)
try_loadstring("\27LJ\2\nZ\0\0\2\0\4\0\t6\0\0\0009\0\1\0)\1\1\0=\1\2\0006\0\0\0009\0\1\0)\1\1\0=\1\3\0K\0\1\0\25rooter_resolve_links\24rooter_silent_chdir\6g\bvim\0", "config", "vim-rooter")
time([[Config for vim-rooter]], false)
-- Config for: nvim-web-devicons
time([[Config for nvim-web-devicons]], true)
try_loadstring("\27LJ\2\nO\0\0\3\0\4\0\a6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\3\0B\0\2\1K\0\1\0\1\0\1\fdefault\2\nsetup\22nvim-web-devicons\frequire\0", "config", "nvim-web-devicons")
time([[Config for nvim-web-devicons]], false)
-- Config for: tcomment_vim
time([[Config for tcomment_vim]], true)
try_loadstring("\27LJ\2\n6\0\0\2\0\4\0\0056\0\0\0009\0\1\0'\1\3\0=\1\2\0K\0\1\0\5\24tcomment_mapleader2\6g\bvim\0", "config", "tcomment_vim")
time([[Config for tcomment_vim]], false)
-- Config for: vim-indent-guides
time([[Config for vim-indent-guides]], true)
try_loadstring('\27LJ\2\n?\0\0\2\0\3\0\0056\0\0\0009\0\1\0)\1\0\0=\1\2\0K\0\1\0"indent_guides_default_mapping\6g\bvim\0', "config", "vim-indent-guides")
time([[Config for vim-indent-guides]], false)
-- Config for: telescope.nvim
time([[Config for telescope.nvim]], true)
try_loadstring("\27LJ\2\n®\t\0\0\b\0/\0K6\0\0\0'\2\1\0B\0\2\0026\1\0\0'\3\2\0B\1\2\0029\1\3\0015\3*\0005\4\5\0005\5\4\0=\5\6\0045\5\b\0005\6\a\0=\6\t\0055\6\n\0=\6\v\5=\5\f\0046\5\0\0'\a\r\0B\5\2\0029\5\14\5=\5\15\0044\5\0\0=\5\16\0046\5\0\0'\a\r\0B\5\2\0029\5\17\5=\5\18\0044\5\0\0=\5\19\0045\5\20\0=\5\21\0044\5\0\0=\5\22\0045\5\23\0=\5\24\0046\5\0\0'\a\25\0B\5\2\0029\5\26\0059\5\27\5=\5\28\0046\5\0\0'\a\25\0B\5\2\0029\5\29\0059\5\27\5=\5\30\0046\5\0\0'\a\25\0B\5\2\0029\5\31\0059\5\27\5=\5 \0046\5\0\0'\a\25\0B\5\2\0029\5!\5=\5!\0045\5'\0005\6#\0009\a\"\0=\a$\0069\a%\0=\a&\6=\6(\5=\5)\4=\4+\3B\1\2\0016\1,\0009\1-\1'\3.\0B\1\2\1K\0\1\0/highlight TelescopeSelection guibg=#3C3836\bcmd\bvim\rdefaults\1\0\0\rmappings\6i\1\0\0\n<C-k>\28move_selection_previous\n<C-j>\1\0\0\24move_selection_next\27buffer_previewer_maker\21qflist_previewer\22vim_buffer_qflist\19grep_previewer\23vim_buffer_vimgrep\19file_previewer\bnew\19vim_buffer_cat\25telescope.previewers\fset_env\1\0\1\14COLORTERM\14truecolor\17path_display\16borderchars\1\t\0\0\b‚îÄ\b‚îÇ\b‚îÄ\b‚îÇ\b‚ï≠\b‚ïÆ\b‚ïØ\b‚ï∞\vborder\19generic_sorter\29get_generic_fuzzy_sorter\25file_ignore_patterns\16file_sorter\19get_fuzzy_file\22telescope.sorters\18layout_config\rvertical\1\0\1\vmirror\1\15horizontal\1\0\0\1\0\1\vmirror\1\22vimgrep_arguments\1\0\n\rwinblend\3\0\20layout_strategy\15horizontal\21sorting_strategy\15descending\23selection_strategy\nreset\17initial_mode\vinsert\17entry_prefix\a  \20selection_caret\a> \18prompt_prefix\a> \ruse_less\2\19color_devicons\2\1\b\0\0\arg\18--color=never\17--no-heading\20--with-filename\18--line-number\r--column\17--smart-case\nsetup\14telescope\22telescope.actions\frequire\0", "config", "telescope.nvim")
time([[Config for telescope.nvim]], false)
-- Config for: nvim-treesitter
time([[Config for nvim-treesitter]], true)
try_loadstring("\27LJ\2\n†\1\0\0\5\0\b\0\v6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\3\0005\3\4\0005\4\5\0=\4\6\3=\3\a\2B\0\2\1K\0\1\0\14highlight\fdisable\1\3\0\0\6c\trust\1\0\1\venable\2\1\0\1\21ensure_installed\15maintained\nsetup\28nvim-treesitter.configs\frequire\0", "config", "nvim-treesitter")
time([[Config for nvim-treesitter]], false)
-- Config for: nvim-cmp
time([[Config for nvim-cmp]], true)
try_loadstring("\27LJ\2\n;\0\1\4\0\4\0\0066\1\0\0009\1\1\0019\1\2\0019\3\3\0B\1\2\1K\0\1\0\tbody\20vsnip#anonymous\afn\bvimá\b\1\0\v\0<\0y6\0\0\0009\0\1\0'\1\3\0=\1\2\0006\0\4\0'\2\5\0B\0\2\0029\1\6\0005\3\n\0005\4\b\0003\5\a\0=\5\t\4=\4\v\0035\4\19\0009\5\f\0009\a\f\0009\a\r\a5\t\16\0009\n\14\0009\n\15\n=\n\17\tB\a\2\0025\b\18\0B\5\3\2=\5\20\0049\5\f\0009\a\f\0009\a\21\a5\t\22\0009\n\14\0009\n\15\n=\n\17\tB\a\2\0025\b\23\0B\5\3\2=\5\24\0049\5\f\0009\a\f\0009\a\25\a)\t¸ˇB\a\2\0025\b\26\0B\5\3\2=\5\27\0049\5\f\0009\a\f\0009\a\25\a)\t\4\0B\a\2\0025\b\28\0B\5\3\2=\5\29\0049\5\f\0009\5\30\0055\a!\0009\b\31\0009\b \b=\b\17\aB\5\2\2=\5\"\4=\4\f\0039\4#\0009\4$\0044\6\3\0005\a%\0>\a\1\0065\a&\0>\a\2\0064\a\3\0005\b'\0>\b\1\aB\4\3\2=\4$\3B\1\2\0019\1\6\0009\1(\1'\3)\0005\4+\0004\5\3\0005\6*\0>\6\1\5=\5$\4B\1\3\0019\1\6\0009\1(\1'\3,\0005\4/\0009\5#\0009\5$\0054\a\3\0005\b-\0>\b\1\a4\b\3\0005\t.\0>\t\1\bB\5\3\2=\5$\4B\1\3\0016\1\4\0'\0030\0B\1\2\0029\2\6\0005\0045\0005\0053\0009\0061\0015\b2\0B\6\2\2=\0064\5=\0056\4B\2\2\0016\2\4\0'\0047\0B\2\2\0029\0028\0026\4\0\0009\0049\0049\4:\0049\4;\4B\4\1\0A\2\0\2K\0\1\0\29make_client_capabilities\rprotocol\blsp\24update_capabilities\17cmp_nvim_lsp\15formatting\1\0\0\vformat\1\0\0\1\0\2\14with_text\1\rmaxwidth\0032\15cmp_format\flspkind\1\0\0\1\0\1\tname\fcmdline\1\0\1\tname\tpath\6:\1\0\0\1\0\1\tname\vbuffer\6/\fcmdline\1\0\1\tname\vbuffer\1\0\1\tname\nvsnip\1\0\1\tname\rnvim_lsp\fsources\vconfig\t<CR>\1\0\1\vselect\2\fReplace\20ConfirmBehavior\fconfirm\n<C-f>\1\3\0\0\6i\6c\n<C-b>\1\3\0\0\6i\6c\16scroll_docs\n<C-k>\1\3\0\0\6i\6c\1\0\0\21select_prev_item\n<C-j>\1\0\0\1\3\0\0\6i\6c\rbehavior\1\0\0\vInsert\19SelectBehavior\21select_next_item\fmapping\fsnippet\1\0\0\vexpand\1\0\0\0\nsetup\bcmp\frequire\26menu,menuone,noselect\16completeopt\6o\bvim\0", "config", "nvim-cmp")
time([[Config for nvim-cmp]], false)

-- Command lazy-loads
time([[Defining lazy-load commands]], true)
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Wall lua require("packer.load")({'vim-eunuch'}, { cmd = "Wall", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file SudoEdit lua require("packer.load")({'vim-eunuch'}, { cmd = "SudoEdit", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file SudoWrite lua require("packer.load")({'vim-eunuch'}, { cmd = "SudoWrite", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file DetectIndent lua require("packer.load")({'detectindent'}, { cmd = "DetectIndent", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Tabularize lua require("packer.load")({'tabular'}, { cmd = "Tabularize", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file StartupTime lua require("packer.load")({'vim-startuptime'}, { cmd = "StartupTime", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Goyo lua require("packer.load")({'goyo.vim'}, { cmd = "Goyo", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Listmaps lua require("packer.load")({'listmaps.vim'}, { cmd = "Listmaps", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file NvimTreeToggle lua require("packer.load")({'nvim-tree.lua'}, { cmd = "NvimTreeToggle", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Move lua require("packer.load")({'vim-eunuch'}, { cmd = "Move", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Git lua require("packer.load")({'vim-fugitive'}, { cmd = "Git", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Remove lua require("packer.load")({'vim-eunuch'}, { cmd = "Remove", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Unlink lua require("packer.load")({'vim-eunuch'}, { cmd = "Unlink", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Gitabra lua require("packer.load")({'gitabra'}, { cmd = "Gitabra", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Rename lua require("packer.load")({'vim-eunuch'}, { cmd = "Rename", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Chmod lua require("packer.load")({'vim-eunuch'}, { cmd = "Chmod", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Mkdir lua require("packer.load")({'vim-eunuch'}, { cmd = "Mkdir", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Find lua require("packer.load")({'vim-eunuch'}, { cmd = "Find", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Locate lua require("packer.load")({'vim-eunuch'}, { cmd = "Locate", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
time([[Defining lazy-load commands]], false)

vim.cmd [[augroup packer_load_aucmds]]
vim.cmd [[au!]]
  -- Filetype lazy-loads
time([[Defining lazy-load filetype autocommands]], true)
vim.cmd [[au FileType html ++once lua require("packer.load")({'emmet-vim'}, { ft = "html" }, _G.packer_plugins)]]
vim.cmd [[au FileType xml ++once lua require("packer.load")({'emmet-vim'}, { ft = "xml" }, _G.packer_plugins)]]
vim.cmd [[au FileType python ++once lua require("packer.load")({'python-syntax', 'vim-markdown', 'vim-flake8'}, { ft = "python" }, _G.packer_plugins)]]
vim.cmd [[au FileType markdown ++once lua require("packer.load")({'vim-livedown'}, { ft = "markdown" }, _G.packer_plugins)]]
vim.cmd [[au FileType fennel ++once lua require("packer.load")({'rainbow', 'aniseed', 'vim-sexp-mappings-for-regular-people', 'fennel.vim', 'vim-sexp', 'conjure'}, { ft = "fennel" }, _G.packer_plugins)]]
vim.cmd [[au FileType nimrod ++once lua require("packer.load")({'nimrod.vim'}, { ft = "nimrod" }, _G.packer_plugins)]]
vim.cmd [[au FileType clojure ++once lua require("packer.load")({'rainbow', 'vim-sexp-mappings-for-regular-people', 'vim-sexp', 'conjure'}, { ft = "clojure" }, _G.packer_plugins)]]
time([[Defining lazy-load filetype autocommands]], false)
vim.cmd("augroup END")
vim.cmd [[augroup filetypedetect]]
time([[Sourcing ftdetect script at: /Users/odie/.local/share/nvim/site/pack/packer/opt/nimrod.vim/ftdetect/nim.vim]], true)
vim.cmd [[source /Users/odie/.local/share/nvim/site/pack/packer/opt/nimrod.vim/ftdetect/nim.vim]]
time([[Sourcing ftdetect script at: /Users/odie/.local/share/nvim/site/pack/packer/opt/nimrod.vim/ftdetect/nim.vim]], false)
time([[Sourcing ftdetect script at: /Users/odie/.local/share/nvim/site/pack/packer/opt/aniseed/ftdetect/fennel.vim]], true)
vim.cmd [[source /Users/odie/.local/share/nvim/site/pack/packer/opt/aniseed/ftdetect/fennel.vim]]
time([[Sourcing ftdetect script at: /Users/odie/.local/share/nvim/site/pack/packer/opt/aniseed/ftdetect/fennel.vim]], false)
time([[Sourcing ftdetect script at: /Users/odie/.local/share/nvim/site/pack/packer/opt/fennel.vim/ftdetect/fennel.vim]], true)
vim.cmd [[source /Users/odie/.local/share/nvim/site/pack/packer/opt/fennel.vim/ftdetect/fennel.vim]]
time([[Sourcing ftdetect script at: /Users/odie/.local/share/nvim/site/pack/packer/opt/fennel.vim/ftdetect/fennel.vim]], false)
time([[Sourcing ftdetect script at: /Users/odie/.local/share/nvim/site/pack/packer/opt/vim-markdown/ftdetect/markdown.vim]], true)
vim.cmd [[source /Users/odie/.local/share/nvim/site/pack/packer/opt/vim-markdown/ftdetect/markdown.vim]]
time([[Sourcing ftdetect script at: /Users/odie/.local/share/nvim/site/pack/packer/opt/vim-markdown/ftdetect/markdown.vim]], false)
vim.cmd("augroup END")
if should_profile then save_profiles() end

end)

if not no_errors then
  vim.api.nvim_command('echohl ErrorMsg | echom "Error in packer_compiled: '..error_msg..'" | echom "Please check your config for correctness" | echohl None')
end
