local wk = require("which-key")

wk.register({
  ['/'] = {'<cmd>call esearch#init()<cr>', 'Search Project' },

  ['<TAB>'] = {'<c-^>','Last Buffer'},

  ['<SPACE>'] = {
    name = "Easy Motion"
  },

  b = {
    name = "buffer",
    b = {'<cmd>Telescope buffers<cr>', 'Buffers'},

    d = {'<cmd>BufferClose<cr>', 'Delete Buffer'},

    D = {'<cmd>BufferClose!<cr>', 'Delete Buffer (force)'},

    p = {'<cmd>bprev<cr>', 'Next buffer'},
    n = {'<cmd>bnext<cr>', 'Prev buffer'},
  },

  D = { '<cmd>Dash<cr>', 'Search in Dash' },
  d = {
    name = 'Diagnostics',
    t = { '<cmd>TroubleToggle<cr>', 'trouble' },
    w = { '<cmd>TroubleToggle workspace_diagnostics<cr>', 'workspace' },
    d = { '<cmd>TroubleToggle document_diagnostics<cr>', 'document' },
    q = { '<cmd>TroubleToggle quickfix<cr>', 'quickfix' },
    l = { '<cmd>TroubleToggle loclist<cr>', 'loclist' },
    r = { '<cmd>TroubleToggle lsp_references<cr>', 'references' },
  },

  f = {
    name = 'files',

    e = {
      name = 'editor',
      d = {'<cmd>e $MYVIMRC<cr>', 'Open Config File'},
      r = {'<cmd>:source $MYVIMRC<cr><cmd>echom "Sourced vimrc"<cr>', 'Reload config file'},
    },
    f = {"<cmd>lua require'odie.telescope'.find_files()<cr>", "Find by name"},
    g = {"<cmd>lua require('telescope.builtin').live_grep({find_command = require'odie.telescope'.find_command})<cr>", "Grep in file"},
    -- t = {'<cmd>NvimTreeToggle<cr>', 'File tree'},
    t = {'<cmd>NvimTreeFindFile<cr>', 'File tree'},
    -- t = {'<cmd>NvimTreeFindFileToggle<cr>', 'File tree'},
    n = {":echo expand('%:p')<cr>", "Show filename"}
  },

  g = {
    name = "git",
    b = {'<cmd>Telescope git_branches<cr>', 'Switch Branch'},
    B = {'<cmd>Gblame<cr>', "Git Blame"},
    s = {'<cmd>Gitabra<cr>', "Gitabra"},
    d = {'<cmd>Gdiff<cr>', "Git Diff"},
    l = {'<cmd>Glog<cr>', "Git Log"},
    c = {'<cmd>Gcommit<cr>', "Git Commit"},
    p = {'<cmd>Gpush<cr>', "Git Push"},
  },

  h = {
    name = 'help',
    t = {'<cmd>Telescope help_tags<cr>', 'Find Tags'},
  },

  n = { '<cmd>NvimTreeToggle<cr>', 'File tree' },

  t = {
    name = "toggle",
    i = {'<cmd>IndentGuidesToggle<cr>', 'Indent guide'},
    L = {'<cmd>set wrap!<cr>', 'Line wrap'},
    l = {'<cmd>set list!<cr>', 'List invisible characters'},
  },

  q = {'<cmd>close<cr>', 'Close window'},

  w = {
    name = "window",
    n = {"<C-w><C-w>", "next"}
  }


}, { prefix = "<leader>" })

wk.register({
  a = {
    name = "align",
    ["="] = {"<cmd>Tabularize /=<cr>", "align ="},
    [":"] = {"<cmd>Tabularize /:<cr>", "align :"},
    ["::"] = {"<cmd>Tabularize /:\zs<cr>", "align ::"},
    [","] = {"<cmd>Tabularize /,<cr>", "align ,"},
    ["|"] = {"<cmd>Tabularize /<cr>", "align |"},
  },
}, {mode = "v", prefix = "<leader>"})
