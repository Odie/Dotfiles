vim.cmd [[packadd packer.nvim]]

-- Automatically recompile plugins.lua on file write
vim.cmd([[
	augroup AutoCompilePlugins
		au!
		autocmd BufWritePost plugins.lua PackerCompile
	augroup end
]])

local fn = vim.fn
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
	packer_bootstrap = fn.system({
		"git",
		"clone",
		"--depth",
		"1",
		"https://github.com/wbthomason/packer.nvim",
		install_path,
	})
end

return require('packer').startup(function(use)
	use 'wbthomason/packer.nvim'

	use {
		'~/dev/vim/gitabra',
		cmd = {"Gitabra"},
		setup = function()
			vim.g.gitabra_dev = 1
		end
	}

	use {
		"folke/trouble.nvim",
		requires = "kyazdani42/nvim-web-devicons",
		config = function()
			require("trouble").setup {}
		end
	}

	use {
		'nvim-treesitter/nvim-treesitter',
		--run = ':TSUpdate',
		config = function()
			require'nvim-treesitter.configs'.setup {
				ensure_installed = {"c", "cpp", "lua"},
				sync_install = false,
				highlight = {
					enable = true,	-- false will disable the whole extension
					disable = { },	-- list of language that will be disabled
					additional_vim_regex_highlighting = false,
				},
			}
		end
	}

	use 'neovim/nvim-lspconfig'
	use {
		'onsails/lspkind-nvim',
		requires = {
			'folke/which-key.nvim',
			'neovim/nvim-lspconfig'
		},
		config = function()
			require('lspkind').init()
			require('lsp/config')
		end
	}
	use 'williamboman/nvim-lsp-installer'

	use 'nvim-lua/lsp-status.nvim'

	use 'L3MON4D3/LuaSnip'
	use 'saadparwaiz1/cmp_luasnip'

	use {
		'hrsh7th/nvim-cmp',
		requires = {
			'onsails/lspkind-nvim',
			'hrsh7th/cmp-nvim-lsp',
			'hrsh7th/cmp-buffer',
			'hrsh7th/cmp-path',
			'hrsh7th/cmp-cmdline',
			-- 'hrsh7th/cmp-vsnip',
			-- 'hrsh7th/vim-vsnip',
		},

		config = function()
			vim.o.completeopt = "menu,menuone,noselect"
			local cmp = require'cmp'
			cmp.setup({
				snippet = {
					-- REQUIRED - you must specify a snippet engine
					expand = function(args)
						-- vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
						require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
						-- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
						-- require'snippy'.expand_snippet(args.body) -- For `snippy` users.
					end,
				},
				mapping = {
					['<C-j>'] = cmp.mapping(cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }), { 'i', 'c' }),
					['<C-k>'] = cmp.mapping(cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }), { 'i', 'c' }),
					['<C-d>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
					['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
					['<CR>'] = cmp.mapping.confirm({
						behavior = cmp.ConfirmBehavior.Replace,
						select = true,
					})
				},
				sources = cmp.config.sources({
					{ name = 'nvim_lsp' },
					-- { name = 'vsnip' }, -- For vsnip users.
					{ name = 'luasnip' }, -- For luasnip users.
					-- { name = 'ultisnips' }, -- For ultisnips users.
					-- { name = 'snippy' }, -- For snippy users.
					{ name = 'buffer' },
					{ name = 'path' },
				})
			})

			-- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
			cmp.setup.cmdline('/', {
				sources = {
					{ name = 'buffer' }
				}
			})

			-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
			cmp.setup.cmdline(':', {
				sources = cmp.config.sources({
					{ name = 'path' }
				}, {
					{ name = 'cmdline' }
				})
			})

			local lspkind = require('lspkind')
			cmp.setup {
				formatting = {
					format = lspkind.cmp_format({with_text = false, maxwidth = 50})
				}
			}

			-- Setup lspconfig.
			local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
			-- Replace <YOUR_LSP_SERVER> with each lsp server you've enabled.
			-- require('lspconfig')['<YOUR_LSP_SERVER>'].setup {
			--	capabilities = capabilities
			-- }
		end
	}

	use {
		"lewis6991/gitsigns.nvim",
		config = function()
			require("gitsigns").setup{ }
		end,
	}

	-- Fast & functional statusline
	use {
		'nvim-lualine/lualine.nvim',
		requires = {'kyazdani42/nvim-web-devicons'},
		config = function()
			local lualine = require('lualine')
			lualine.setup()
			lualine.theme = 'gruvbox'
		end
	}

	-- Visual file tabs
	use {
		'romgrk/barbar.nvim',
		requires = {'kyazdani42/nvim-web-devicons'}
	}

	use {
		'kyazdani42/nvim-web-devicons',
		config = function()
			require'nvim-web-devicons'.setup({default=true})
		end
	}

	-- File explorer
	--	"<Leader>n" to toggle on and off
	use {
		'kyazdani42/nvim-tree.lua',
		cmd = {"NvimTreeToggle"},
		config = function()
			require'nvim-tree'.setup {
				disable_netrw				= true,
				hijack_netrw				= true,
				open_on_setup				= false,
				ignore_ft_on_setup	= {},
				auto_close					= false,
				open_on_tab					= false,
				hijack_cursor				= false,
				update_cwd					= false,
				update_to_buf_dir		= {
					enable = true,
					auto_open = true,
				},
				diagnostics = {
					enable = false,
					icons = {
						hint = "",
						info = "",
						warning = "",
						error = "",
					}
				},
				update_focused_file = {
					enable			= false,
					update_cwd	= false,
					ignore_list = {}
				},
				system_open = {
					cmd  = nil,
					args = {}
				},
				filters = {
					dotfiles = false,
					custom = {}
				},
				view = {
					width = 30,
					height = 30,
					hide_root_folder = false,
					side = 'left',
					auto_resize = false,
					mappings = {
						custom_only = false,
						list = {}
					}
				}
			}

			vim.g.nvim_tree_show_icons = {
					git = 1,
					folders = 1,
					files = 1
			}
		end,
	}

	use {
		'folke/which-key.nvim',
		config = function()
			require('config/which-key')
		end
	}

	-- use 'tpope/vim-sleuth'
	use {
		'ciaranm/detectindent',
		cmd = {"DetectIndent"},
		setup = function()
			vim.g.detectindent_preferred_indent		= 4
			vim.g.detectindent_preferred_expandtab	= 1 -- Prefer noexpandtab
			vim.cmd([[
				augroup AutoDetectIndent
					au!
					autocmd BufReadPost * :DetectIndent
				augroup end
			]])
		end
	}

	-- Startup time visualization
	use {
		'dstein64/vim-startuptime',
		cmd = "StartupTime",
	}

	-- Distraction free writing in vim
	use {
		'junegunn/goyo.vim',
		cmd = "Goyo",
	}

	-- ":Listmaps" lists all mappings in all sourced files in a separate buffer
	use {
		'vim-scripts/listmaps.vim',
		cmd = "Listmaps",
	}

	-- Jump between files faster
	--	 control-p to jump between files
	use 'nvim-lua/popup.nvim'
	use {
		'nvim-telescope/telescope.nvim',
		requires = {'nvim-lua/plenary.nvim'},
		config = function()
			local actions = require('telescope.actions')
			require('telescope').setup{
				defaults = {
					vimgrep_arguments = {
						'rg',
						'--color=never',
						'--no-heading',
						'--with-filename',
						'--line-number',
						'--column',
						'--smart-case'
					},
					prompt_prefix = "> ",
					selection_caret = "> ",
					entry_prefix = "	",
					initial_mode = "insert",
					selection_strategy = "reset",
					sorting_strategy = "descending",
					layout_strategy = "horizontal",
					layout_config = {
						horizontal = {
							mirror = false,
						},
						vertical = {
							mirror = false,
						},
					},
					file_sorter =  require'telescope.sorters'.get_fuzzy_file,
					file_ignore_patterns = {},
					generic_sorter =	require'telescope.sorters'.get_generic_fuzzy_sorter,
					winblend = 0,
					border = {},
					borderchars = { '─', '│', '─', '│', '╭', '╮', '╯', '╰' },
					color_devicons = true,
					use_less = true,
					path_display = {},
					set_env = { ['COLORTERM'] = 'truecolor' }, -- default = nil,
					file_previewer = require'telescope.previewers'.vim_buffer_cat.new,
					grep_previewer = require'telescope.previewers'.vim_buffer_vimgrep.new,
					qflist_previewer = require'telescope.previewers'.vim_buffer_qflist.new,

					-- Developer configurations: Not meant for general override
					buffer_previewer_maker = require'telescope.previewers'.buffer_previewer_maker,

					mappings = {
						i = {
							["<C-j>"] = actions.move_selection_next,
							["<C-k>"] = actions.move_selection_previous
						}
					}
				}
			}
			vim.cmd([[highlight TelescopeSelection guibg=#3C3836]])
		end
	}

	-- use 'sheerun/vim-polyglot'

	-- Language specific % motion
	use 'andymass/vim-matchup'

	-- Set working directory automatically to git/project root
	use {
		'airblade/vim-rooter',
		config = function()
			vim.g.rooter_silent_chdir = 1
			vim.g.rooter_resolve_links = 1
		end
	}

	--- Unix helpers
	--- ":SudoWrite" to write a file with sudo
	use {
		'tpope/vim-eunuch',
		cmd = {"Remove", "Unlink", "Move", "Rename", "Chmod", "Mkdir", "Find", "Locate", "Wall", "SudoEdit", "SudoWrite"}
	}

	use {
		'neomake/neomake',
		config = function()
			vim.cmd([[
				autocmd! BufWritePost * Neomake
			]])
		end
	}

	--- Automatically make containing directory when opening a new buffer
	use  'jordwalke/VimAutoMakeDirectory'

	use 'xolox/vim-misc'

	use {
		'godlygeek/tabular',
		cmd = "Tabularize",
	}

	--- "<Leader> q" quits the buffer
	use 'moll/vim-bbye'

	-- Live markdown preview
	use {
		'shime/vim-livedown',
		ft = "markdown",
		config = function()
			vim.g.livedown_autorun = 1
			vim.g.livedown_open = 1
			vim.g.livedown_port = 2000
		end
	}

	--- Html structure generation
	use {
		'mattn/emmet-vim',
		ft = {"html", "xml"},
	}

	--- Switch to alternative files quickly
	use {
		'nacitar/a.vim',
		--event = 'BufReadPre',
	}

	--- Adds various ex command shortcuts
	--- [b and ]b to switch to previous and next buffer
	use 'tpope/vim-unimpaired'

	--- Jump to location in buffer faster
	-- use 'Lokaltog/vim-easymotion'
	-- use {
	-- 	'phaazon/hop.nvim',
	-- 	branch = 'v1', -- optional but strongly recommended
	-- 	config = function()
	-- 		require'hop'.setup { }
    --
	-- 		nnoremap('t', "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true })<cr>")
	-- 		nnoremap('T', "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true })<cr>")
	-- 		nnoremap('f', "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true })<cr>")
	-- 		nnoremap('F', "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true })<cr>")
	-- 		nnoremap('<leader><leader>t', "<cmd>HopChar1<cr>")
	-- 		nnoremap('<leader><leader>f', "<cmd>HopChar1<cr>")
	-- 		nnoremap('<leader><leader>w', "<cmd>HopWord<cr>")
	-- 		nnoremap('<leader><leader>/', "<cmd>HopPattern<cr>")
    --
	-- 	end
	-- }
	use 'ggandor/lightspeed.nvim'

	use 'terryma/vim-multiple-cursors'
	use 'tpope/vim-surround'

	--- Quickly comment/uncomment lines
	use {
		'tomtom/tcomment_vim',
		config = function()
			vim.g.tcomment_mapleader2 = ''
		end
	}

	use 'tpope/vim-repeat'

	--- Fancier start page
	use 'mhinz/vim-startify'

	--- Visually display indent levels
	use {
		'nathanaelkane/vim-indent-guides',
		config = function()
			vim.g.indent_guides_default_mapping = 0
		end
	}

	use {
		'tpope/vim-fugitive',
		cmd = "Git"
	}

	use {
		'norcalli/nvim-colorizer.lua',
		config = function()
			require'colorizer'.setup()
		end
	}

	use 'AndrewRadev/bufferize.vim'

	--- Search documentation in Dash.app
	use {
		'rizzatti/dash.vim',
		cmd = {'Dash'}
	}

	use {
		'sindrets/diffview.nvim',
		requires = 'nvim-lua/plenary.nvim'
	}
	---------------------------------------------------------------------------
	-- Lispy languages
	---------------------------------------------------------------------------
	use {
		'Olical/aniseed',
		ft={"fennel"},
	}
	use {
		'Olical/conjure',
		ft={"clojure", "fennel"},
		config = function()
			vim.cmd([[let g:conjure#log#botright = 1]])
			-- vim.g.conjure.log.botright = true
		end
	}
	use {
		'bakpakin/fennel.vim',
		ft={"fennel"}
	}

	use {
		'luochen1990/rainbow',
		ft={"clojure", "fennel"}
	}
	use {
		'guns/vim-sexp',
		ft={"clojure", "fennel"},
		config = function()
			vim.g.sexp_mappings = {
				sexp_outer_list=				'af',
				sexp_inner_list=				'if',
				sexp_outer_top_list=			'aF',
				sexp_inner_top_list=			'iF',
				sexp_outer_string=				'as',
				sexp_inner_string=				'is',
				sexp_outer_element=				'ae',
				sexp_inner_element=				'ie',
				sexp_move_to_prev_bracket=		'(',
				sexp_move_to_next_bracket=		')',
				sexp_move_to_prev_element_head= '<M-b>',
				sexp_move_to_next_element_head= '<M-w>',
				sexp_move_to_prev_element_tail= 'g<M-e>',
				sexp_move_to_next_element_tail= '<M-e>',
				sexp_flow_to_prev_close=		'<M-[>',
				sexp_flow_to_next_open=			'<M-]>',
				sexp_flow_to_prev_open=			'<M-{>',
				sexp_flow_to_next_close=		'<M-}>',
				sexp_flow_to_prev_leaf_head=	'<M-S-b>',
				sexp_flow_to_next_leaf_head=	'<M-S-w>',
				sexp_flow_to_prev_leaf_tail=	'<M-S-g>',
				sexp_flow_to_next_leaf_tail=	'<M-S-e>',
				sexp_move_to_prev_top_element=	'[[',
				sexp_move_to_next_top_element=	']]',
				sexp_select_prev_element=		'[e',
				sexp_select_next_element=		']e',
				sexp_indent=					'==',
				sexp_indent_top=				'=-',
				sexp_round_head_wrap_list=		'',
				sexp_round_tail_wrap_list=		'',
				sexp_square_head_wrap_list=		'',
				sexp_square_tail_wrap_list=		'',
				sexp_curly_head_wrap_list=		'',
				sexp_curly_tail_wrap_list=		'',
				sexp_round_head_wrap_element=	'',
				sexp_round_tail_wrap_element=	'',
				sexp_square_head_wrap_element=	'',
				sexp_square_tail_wrap_element=	'',
				sexp_curly_head_wrap_element=	'',
				sexp_curly_tail_wrap_element=	'',
				sexp_insert_at_list_head=		'',
				sexp_insert_at_list_tail=		'',
				sexp_splice_list=				'',
				sexp_convolute=					'',
				sexp_raise_list=				'',
				sexp_raise_element=				'',
				sexp_swap_list_backward=		'<M-k>',
				sexp_swap_list_forward=			'<M-j>',
				sexp_swap_element_backward=		'<M-h>',
				sexp_swap_element_forward=		'<M-l>',
				sexp_emit_head_element=			'<M-S-j>',
				sexp_emit_tail_element=			'<M-S-k>',
				sexp_capture_prev_element=		'<M-S-h>',
				sexp_capture_next_element=		'<M-S-l>',
			}
		end
	}

	use {
		'tpope/vim-sexp-mappings-for-regular-people',
		ft={"clojure", "fennel"},
	}

	use {
		'eugen0329/vim-esearch',
		config = function()
			vim.g.esearch = {
				default_mappings = 0,
				win_map = {
					{'n',  'R',		 '<plug>(esearch-win-reload)',					 },
					{'n',  't',		 '<plug>(esearch-win-tabopen)',					 },
					{'n',  'T',		 '<plug>(esearch-win-tabopen:stay)',		 },
					{'n',  'o',		 '<plug>(esearch-win-split)',						 },
					{'n',  'O',		 '<plug>(esearch-win-split:reuse:stay)', },
					{'n',  's',		 '<plug>(esearch-win-vsplit)',					 },
					{'n',  'S',		 '<plug>(esearch-win-vsplit:reuse:stay)',},
					{'n',  '<cr>', '<plug>(esearch-win-open)',						 },
					{'n',  'p',		 '<plug>(esearch-win-preview)',					 },
					{'n',  'P',		 '100<plug>(esearch-win-preview:enter)', },
					{'n',  '<esc>','<plug>(esearch-win-preview:close)',		 },
					{' ',  'J',		 '<plug>(esearch-win-jump:entry:down)'	 },
					{' ',  'K',		 '<plug>(esearch-win-jump:entry:up)'		 },
					{' ',  '}',		 '<plug>(esearch-win-jump:filename:down)'},
					{' ',  '{',		 '<plug>(esearch-win-jump:filename:up)'  },
					{' ',  ')',		 '<plug>(esearch-win-jump:dirname:down)' },
					{' ',  '(',		 '<plug>(esearch-win-jump:dirname:up)'	 },
					{'ov', 'im',	 '<plug>(textobj-esearch-match-i)',			 },
					{'ov', 'am',	 '<plug>(textobj-esearch-match-a)',			 },
					{'ic', '<cr>', '<plug>(esearch-cr)', {nowait= 1}			 },
					{'n',  'I',		 '<plug>(esearch-I)'										 },
					{'x',  'x',		 '<plug>(esearch-d)'										 },
					{'nx', 'd',		 '<plug>(esearch-d)'										 },
					{'n',  'dd',	 '<plug>(esearch-dd)'										 },
					{'nx', 'c',		 '<plug>(esearch-c)'										 },
					{'n',  'cc',	 '<plug>(esearch-cc)'										 },
					{'nx', 'C',		 '<plug>(esearch-C)'										 },
					{'nx', 'D',		 '<plug>(esearch-D)'										 },
					{'x',  's',		 '<plug>(esearch-c)'										 },
					{'n',  '.',		 '<plug>(esearch-.)'										 },
					{'n',  '@:',	 '<plug>(esearch-@:)'										 },
					{'n', 'za',		 '<plug>(esearch-za)'										 },
					{'n', 'zc',		 '<plug>(esearch-zc)'										 },
					{'n', 'zM',		 '<plug>(esearch-zM)'										 },
				}
			}
		end
	}


	-------------------------------------------------------------------------------
	-- Color schemes
	-------------------------------------------------------------------------------
	use 'flazz/vim-colorschemes'
	use 'chriskempson/base16-vim'
	use 'morhetz/gruvbox'
	use 'sainnhe/gruvbox-material'

	-------------------------------------------------------------------------------
	-- Syntax highlighting
	-------------------------------------------------------------------------------

	--- Python syntax support
	use {
		'hdima/python-syntax',
		ft = "python",
	}

	--- Syntax highlighting for blade templating language
	use 'xsbeats/vim-blade'

	--- Syntax highlighting for nimrod files
	use {
		'zah/nimrod.vim',
		ft = "nimrod",
	}

	--- Syntax highlighting for less files
	use 'groenewege/vim-less'

	--- Syntax highlighting for markdown files
	use {
		'tpope/vim-markdown',
		ft = "python",
	}

	--- Python Linter
	--- Requires flake8
	-- use {
	-- 	'nvie/vim-flake8',
	-- 	ft = "python",
	-- }

	--- Syntax highlighting for swift
	use 'keith/swift.vim'


	use {
		"ethanholz/nvim-lastplace",
		event = "BufRead",
		config = function()
			require("nvim-lastplace").setup {
				lastplace_ignore_buftype = { "quickfix", "nofile", "help" },
				lastplace_ignore_filetype = { "gitcommit", "gitrebase", "svn", "hgcommit" },
				lastplace_open_folds = true,
			}
		end,
	}

	-- Automatically set up your configuration after cloning packer.nvim
	-- Put this at the end after all plugins
	if packer_bootstrap then
		require("packer").sync()
	end
end)
