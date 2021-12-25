local wk = require("which-key")

wk.register({
  ["/"] = {'<cmd>call esearch#init()<cr>', 'Search Project' },

  ['<TAB>'] = {'<c-^>','Last Buffer'},
  b = {
    name = "buffer",
    b = {'<cmd>Telescope buffers<cr>', 'Buffers'},

    d = {'<cmd>BufferClose<cr>', 'Delete Buffer'},

    D = {'<cmd>BufferClose!<cr>', 'Delete Buffer (force)'},

    p = {'<cmd>bprev<cr>', 'Next buffer'},
    n = {'<cmd>bnext<cr>', 'Prev buffer'},
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
    t = {'<cmd>NvimTreeToggle<cr>', 'File tree'},
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
