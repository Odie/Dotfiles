
"-------------------------------------------------------------------------------
" Bundle definitions
"-------------------------------------------------------------------------------
call plug#begin('~/.vim/plugged')

Plug 'liuchengxu/vim-which-key'

""" ":Listmaps" lists all mappings in all sourced files in a separate buffer
Plug 'vim-scripts/listmaps.vim'

Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

""" Better looking status line
Plug 'hoob3rt/lualine.nvim'
Plug 'kyazdani42/nvim-web-devicons'

Plug 'romgrk/barbar.nvim'

" Set working directory automatically to git/project root
Plug 'airblade/vim-rooter'
let g:rooter_silent_chdir = 1
let g:rooter_resolve_links = 1

""" control-p to jump between files
""" Commandline utility written in Go
""" Requires separate installation
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'

""" Auto-completion & snippets
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'sheerun/vim-polyglot'

Plug 'nvim-lua/completion-nvim'
Plug 'neovim/nvim-lspconfig'
Plug 'onsails/lspkind-nvim'

"---------------------------------------------------------------------------
" Aniseed
"---------------------------------------------------------------------------
Plug 'Olical/aniseed', { 'tag': 'v3.15.0' }
Plug 'Olical/conjure', {'tag': 'v4.14.1'}
Plug 'bakpakin/fennel.vim' " Fennel syntax highlighting
let g:aniseed#env = v:true

"---------------------------------------------------------------------------
" Clojure plugins
"---------------------------------------------------------------------------
Plug 'luochen1990/rainbow', 	{'for': 'clojure'}
Plug 'guns/vim-sexp',    			{'for': ['clojure', 'fennel']}
let g:sexp_mappings = {
    \ 'sexp_outer_list':                'af',
    \ 'sexp_inner_list':                'if',
    \ 'sexp_outer_top_list':            'aF',
    \ 'sexp_inner_top_list':            'iF',
    \ 'sexp_outer_string':              'as',
    \ 'sexp_inner_string':              'is',
    \ 'sexp_outer_element':             'ae',
    \ 'sexp_inner_element':             'ie',
    \ 'sexp_move_to_prev_bracket':      '(',
    \ 'sexp_move_to_next_bracket':      ')',
    \ 'sexp_move_to_prev_element_head': '<M-b>',
    \ 'sexp_move_to_next_element_head': '<M-w>',
    \ 'sexp_move_to_prev_element_tail': 'g<M-e>',
    \ 'sexp_move_to_next_element_tail': '<M-e>',
    \ 'sexp_flow_to_prev_close':        '<M-[>',
    \ 'sexp_flow_to_next_open':         '<M-]>',
    \ 'sexp_flow_to_prev_open':         '<M-{>',
    \ 'sexp_flow_to_next_close':        '<M-}>',
    \ 'sexp_flow_to_prev_leaf_head':    '<M-S-b>',
    \ 'sexp_flow_to_next_leaf_head':    '<M-S-w>',
    \ 'sexp_flow_to_prev_leaf_tail':    '<M-S-g>',
    \ 'sexp_flow_to_next_leaf_tail':    '<M-S-e>',
    \ 'sexp_move_to_prev_top_element':  '[[',
    \ 'sexp_move_to_next_top_element':  ']]',
    \ 'sexp_select_prev_element':       '[e',
    \ 'sexp_select_next_element':       ']e',
    \ 'sexp_indent':                    '==',
    \ 'sexp_indent_top':                '=-',
    \ 'sexp_round_head_wrap_list':      '',
    \ 'sexp_round_tail_wrap_list':      '',
    \ 'sexp_square_head_wrap_list':     '',
    \ 'sexp_square_tail_wrap_list':     '',
    \ 'sexp_curly_head_wrap_list':      '',
    \ 'sexp_curly_tail_wrap_list':      '',
    \ 'sexp_round_head_wrap_element':   '',
    \ 'sexp_round_tail_wrap_element':   '',
    \ 'sexp_square_head_wrap_element':  '',
    \ 'sexp_square_tail_wrap_element':  '',
    \ 'sexp_curly_head_wrap_element':   '',
    \ 'sexp_curly_tail_wrap_element':   '',
    \ 'sexp_insert_at_list_head':       '',
    \ 'sexp_insert_at_list_tail':       '',
    \ 'sexp_splice_list':               '',
    \ 'sexp_convolute':                 '',
    \ 'sexp_raise_list':                '',
    \ 'sexp_raise_element':             '',
    \ 'sexp_swap_list_backward':        '<M-k>',
    \ 'sexp_swap_list_forward':         '<M-j>',
    \ 'sexp_swap_element_backward':     '<M-h>',
    \ 'sexp_swap_element_forward':      '<M-l>',
    \ 'sexp_emit_head_element':         '<M-S-j>',
    \ 'sexp_emit_tail_element':         '<M-S-k>',
    \ 'sexp_capture_prev_element':      '<M-S-h>',
    \ 'sexp_capture_next_element':      '<M-S-l>',
    \ }


Plug 'tpope/vim-sexp-mappings-for-regular-people', 	{'for': ['clojure', 'fennel']}
Plug 'liquidz/vim-iced', 			{'for': 'clojure'}
let g:iced_enable_default_key_mappings = v:false
Plug 'liquidz/vim-iced-coc-source', 								{'for': 'clojure'}


""" Unix helpers
""" ":SudoWrite" to write a file with sudo
""" Other available commands:
"""   :Remove, :Unlink, :Move, Rename, :Chmod, :Mkdir, :Find, :Locate,
"""   :Wall, :SudoEdit
Plug 'tpope/vim-eunuch'

""" File explorer
""" "<Leader>n" to toggle on and off
Plug 'ms-jpq/chadtree', {'branch': 'chad', 'do': 'python3 -m chadtree deps'}
augroup ChadtreeHijackNetrw
	autocmd!
  autocmd VimEnter * silent! autocmd! FileExplorer
  au BufEnter,VimEnter * if isdirectory(expand('%:p')) | echo "entered!" | execute "CHADopen --always-focus ".expand('%:p') | endif
augroup END

""" Syntax checking
Plug 'benekastah/neomake'
autocmd! BufWritePost * Neomake

""" Better search support via ag
""" ":Ag" and ":Ags" to search
Plug 'gabesoft/vim-ags'
let g:ags_agexe = 'rg'
let g:ags_agargs = {
  \ '--column'         : ['', ''],
  \ '--line-number'    : ['', ''],
  \ '--context'        : ['g:ags_agcontext', '-C'],
  \ '--max-count'      : ['g:ags_agmaxcount', ''],
  \ '--heading'        : ['',''],
  \ '--smart-case'     : ['','-S'],
  \ '--color'          : ['always',''],
  \ '--colors'         : [['match:fg:green', 'match:bg:black', 'match:style:nobold', 'path:fg:red', 'path:style:bold', 'line:fg:black', 'line:style:bold'] ,''],
  \ }

""" Better project-wide search
let g:esearch = {
      \ 'default_mappings' : 0,
      \ 'win_map' : [
      \  ['n',  'R',    '<plug>(esearch-win-reload)',           ],
      \  ['n',  't',    '<plug>(esearch-win-tabopen)',          ],
      \  ['n',  'T',    '<plug>(esearch-win-tabopen:stay)',     ],
      \  ['n',  'o',    '<plug>(esearch-win-split)',            ],
      \  ['n',  'O',    '<plug>(esearch-win-split:reuse:stay)', ],
      \  ['n',  's',    '<plug>(esearch-win-vsplit)',           ],
      \  ['n',  'S',    '<plug>(esearch-win-vsplit:reuse:stay)',],
      \  ['n',  '<cr>', '<plug>(esearch-win-open)',             ],
      \  ['n',  'p',    '<plug>(esearch-win-preview)',          ],
      \  ['n',  'P',    '100<plug>(esearch-win-preview:enter)', ],
      \  ['n',  '<esc>','<plug>(esearch-win-preview:close)',    ],
      \  [' ',  'J',    '<plug>(esearch-win-jump:entry:down)'   ],
      \  [' ',  'K',    '<plug>(esearch-win-jump:entry:up)'     ],
      \  [' ',  '}',    '<plug>(esearch-win-jump:filename:down)'],
      \  [' ',  '{',    '<plug>(esearch-win-jump:filename:up)'  ],
      \  [' ',  ')',    '<plug>(esearch-win-jump:dirname:down)' ],
      \  [' ',  '(',    '<plug>(esearch-win-jump:dirname:up)'   ],
      \  ['ov', 'im',   '<plug>(textobj-esearch-match-i)',      ],
      \  ['ov', 'am',   '<plug>(textobj-esearch-match-a)',      ],
      \  ['ic', '<cr>', '<plug>(esearch-cr)', {'nowait': 1}     ],
      \  ['n',  'I',    '<plug>(esearch-I)'                     ],
      \  ['x',  'x',    '<plug>(esearch-d)'                     ],
      \  ['nx', 'd',    '<plug>(esearch-d)'                     ],
      \  ['n',  'dd',   '<plug>(esearch-dd)'                    ],
      \  ['nx', 'c',    '<plug>(esearch-c)'                     ],
      \  ['n',  'cc',   '<plug>(esearch-cc)'                    ],
      \  ['nx', 'C',    '<plug>(esearch-C)'                     ],
      \  ['nx', 'D',    '<plug>(esearch-D)'                     ],
      \  ['x',  's',    '<plug>(esearch-c)'                     ],
      \  ['n',  '.',    '<plug>(esearch-.)'                     ],
      \  ['n',  '@:',   '<plug>(esearch-@:)'                    ],
      \  ['n', 'za',    '<plug>(esearch-za)'                    ],
      \  ['n', 'zc',    '<plug>(esearch-zc)'                    ],
      \  ['n', 'zM',    '<plug>(esearch-zM)'                    ],
      \],
      \}
Plug   'eugen0329/vim-esearch'

""" Adds various ex command shortcuts
""" [b and ]b to switch to previous and next buffer
Plug 'tpope/vim-unimpaired'

""" Jump to location in buffer faster
Plug 'Lokaltog/vim-easymotion'

Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-surround'

""" Quickly comment/uncomment lines
Plug 'tomtom/tcomment_vim'
let g:tcomment_mapleader2 = ''

Plug 'tpope/vim-repeat'

"-------------------------------------------------------------------------------
" Color schemes
"-------------------------------------------------------------------------------
Plug 'flazz/vim-colorschemes'
Plug 'chriskempson/base16-vim'
Plug 'morhetz/gruvbox'
Plug 'sainnhe/gruvbox-material'

""" Automatically make containing directory when opening a new buffer
Plug 'jordwalke/VimAutoMakeDirectory'

""" Generate tasklist by searching for FIXME, TODO, and XXX
" Plug 'vim-scripts/TaskList.vim'

Plug 'xolox/vim-misc'

""" Automate tag file generation
" Plug 'ludovicchabant/vim-gutentags'

""" Shortcuts for aligning text
Plug 'godlygeek/tabular'
:vmap <silent> <Leader>a=  :Tabularize /= <cr>
:vmap <silent> <Leader>a:  :Tabularize /: <cr>
:vmap <silent> <Leader>a:: :Tabularize /:\zs <cr>
:vmap <silent> <Leader>a,  :Tabularize /, <cr>
:vmap <silent> <Leader>a<Bar> :Tabularize /

""" "<Leader> q" quits the buffer
Plug 'moll/vim-bbye'

""" Show live Markdown preview in browser
Plug 'shime/vim-livedown'
let g:livedown_autorun = 1
let g:livedown_open = 1
let g:livedown_port = 2000

""" Html structure generation
Plug 'mattn/emmet-vim'

""" Matches opening and closing html tags
Plug 'vim-scripts/matchit.zip'

""" Switch to alternative files quickly
Plug 'nacitar/a.vim'

""" Visually display indent levels
Plug 'nathanaelkane/vim-indent-guides'
let g:indent_guides_default_mapping = 0

""" Distraction free writing in vim
""" ":Goyo" to toggle
Plug 'junegunn/goyo.vim'

""" Search documentation in Dash.app
Plug 'rizzatti/dash.vim'
:nmap <silent> <leader>d <Plug>DashSearch

""" Git integration with vim
Plug 'tpope/vim-fugitive'
Plug 'lambdalisue/gina.vim'
Plug 'jreybert/vimagit'

Plug '~/dev/vim/luajob'

""" Fancier start page
Plug 'mhinz/vim-startify'

""" Visualize colors
Plug 'norcalli/nvim-colorizer.lua'

"-------------------------------------------------------------------------------
" Syntax highlighting
"-------------------------------------------------------------------------------

""" Python syntax support
Plug 'hdima/python-syntax'

""" Syntax highlighting for blade templating language
Plug 'xsbeats/vim-blade'

""" Syntax highlighting for nimrod files
Plug 'zah/nimrod.vim'

""" Syntax highlighting for less files
Plug 'groenewege/vim-less'

""" Syntax highlighting for markdown files
Plug 'tpope/vim-markdown'

""" Python Linter
""" Requires flake8
Plug 'nvie/vim-flake8'

""" Syntax highlighting for swift
Plug 'keith/swift.vim'

Plug '~/dev/vim/tigam'
let g:tigma_dev = 1

call plug#end()
filetype plugin indent on
