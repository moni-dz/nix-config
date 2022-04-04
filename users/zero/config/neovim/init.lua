vim.api.nvim_command("packadd impatient.nvim")
vim.api.nvim_command("packadd packer.nvim")

require("impatient")

vim.o.clipboard = "unnamedplus"
vim.o.completeopt = "menu,menuone,noselect"
vim.o.guifont = "monospace:h11"
vim.o.guicursor = "a:ver25-iCursor"
vim.o.laststatus = "0"
vim.o.mouse = "a"
vim.o.modelines = "0"
vim.o.ruler = false
vim.o.number = false
vim.o.termguicolors = true

require("packer").startup(function()
  use "LnL7/vim-nix"

  use {
    "joshdick/onedark.vim",
    config = function() vim.cmd "colorscheme onedark" end
  }
end)
