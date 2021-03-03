vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function()
 	use {'wbthomason/packer.nvim', opt = true}

  use {'~/dev/vim/gitabra',
			 cmd = {"Gitabra"},
       rocks = {'chronos'}}
end)
