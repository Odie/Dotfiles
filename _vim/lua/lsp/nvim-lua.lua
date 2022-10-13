local system_name
if vim.fn.has("mac") == 1 then
  system_name = "macOS"
elseif vim.fn.has("unix") == 1 then
  system_name = "Linux"
elseif vim.fn.has('win32') == 1 then
  system_name = "Windows"
else
  print("Unsupported system for sumneko")
end

local function setup(custom_attach_fn)
  -- set the path to the sumneko installation; if you previously installed via the now deprecated :LspInstall, use
  -- local sumneko_root_path = vim.fn.expand('~/.tools')..'/nvim/lsp/lua-language-server'

  -- local sumneko_root_path = vim.fn.expand('~/.local/share')..'/nvim/lsp_servers/sumneko_lua'
  -- local sumneko_binary = sumneko_root_path.."/bin/"..system_name.."/lua-language-server"

  require'lspconfig'.sumneko_lua.setup {
    on_attach = custom_attach_fn,
    --cmd = {sumneko_binary, "-E", sumneko_root_path .. "/main.lua"};
    cmd = {vim.fn.stdpath("data").."/lsp_servers/sumneko_lua/extension/server/bin/lua-language-server"},

    settings = {
      Lua = {
        runtime = {
          -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
          version = 'LuaJIT',
          -- -- Setup your lua path
          -- path = vim.split(package.path, ';'),
        },
        diagnostics = {
          -- Get the language server to recognize the `vim` global
          globals = {
            -- vim
            "vim",

            -- Busted
            "describe",
            "it",
            "before_each",
            "after_each",
            "teardown",
            "pending",
            "clear",

            -- Colorbuddy
            "Color",
            "c",
            "Group",
            "g",
            "s",

            -- Custom
            "RELOAD",
          },
        },
        workspace = {
          -- Make the server aware of Neovim runtime files
          library = vim.api.nvim_get_runtime_file("", true),

        },
      },
    },
  }
end

return {setup = setup}
