vim.api.nvim_command("packadd impatient.nvim")
vim.api.nvim_command("packadd packer.nvim")

require("impatient")

vim.o.clipboard = "unnamedplus"
vim.o.completeopt = "menu,menuone,noselect"
vim.o.guifont = "monospace:h11"
vim.o.guicursor = "n-c-v:ver30-iCursor"
vim.o.laststatus = "0"
vim.o.mouse = "a"
vim.o.modelines = "0"
vim.o.ruler = false
vim.o.number = false
vim.o.termguicolors = true

require("packer").startup(function()
  use "LnL7/vim-nix"

  use {
    "neovim/nvim-lspconfig",
    requires = {{ "ms-jpq/coq_nvim" }, { "glepnir/lspsaga.nvim" }},

    config = function()
      vim.g.coq_settings = { auto_start = "shut-up" }

      local lsp = require("lspconfig")
      local coq = require("coq")

      lsp.rnix.setup({})
      lsp.rnix.setup(coq.lsp_ensure_capabilities({}))

      require("lspsaga").init_lsp_saga()
    end
  }

  use {
    "marko-cerovac/material.nvim",

    config = function()
      vim.g.material_style = "darker"

      require("material").setup({
        high_visibility = {
          lighter = false,
	  darker = true
	},
      })

      vim.cmd "colorscheme material"
    end
  }

  use {
    "norcalli/nvim-colorizer.lua",

    config = function()
      require("colorizer").setup({ "*" }, {
        RRGGBBAA = true;
	css = true;
	mode = "background";
      })
    end
  }
end)
