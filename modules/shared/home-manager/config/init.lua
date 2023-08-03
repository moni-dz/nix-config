vim.g.mapleader = " "
vim.opt.termguicolors = true
vim.opt.clipboard = "unnamedplus"
vim.opt.mouse = "a"
vim.opt.encoding = "utf-8"
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.cmdheight = 0
vim.opt.laststatus = 0

local auto_dark_mode = require("auto-dark-mode")

auto_dark_mode.setup({
	update_interval = 500,

	set_dark_mode = function()
		vim.api.nvim_set_option('background', 'dark')
		vim.cmd('colorscheme rose-pine-moon')
	end,

	set_light_mode = function()
		vim.api.nvim_set_option('background', 'light')
		vim.cmd('colorscheme rose-pine-dawn')
	end,
})

auto_dark_mode.init()

local cmp = require("cmp")

cmp.setup({
  preselect = cmp.PreselectMode.None,
  snippet = {
    expand = function(args)
      vim.fn["vsnip#anonymous"](args.body)
    end,
  },
  mapping = {
    ["<C-p>"] = cmp.mapping.select_prev_item(),
    ["<C-n>"] = cmp.mapping.select_next_item(),
    -- Add tab support
    ["<S-Tab>"] = cmp.mapping.select_prev_item(),
    ["<Tab>"] = cmp.mapping.select_next_item(),
    ["<C-d>"] = cmp.mapping.scroll_docs(-4),
    ["<C-f>"] = cmp.mapping.scroll_docs(4),
    ["<C-Space>"] = cmp.mapping.complete(),
    ["<C-e>"] = cmp.mapping.close(),
    ["<CR>"] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Insert,
      select = true,
    }),
  },

  -- Installed sources
  sources = {
    { name = "nvim_lsp" },
    { name = "vsnip" },
    { name = "path" },
    { name = "buffer" },
  },
})

local ih = require("inlay-hints")

require("rust-tools").setup({
	tools = {
		on_initialized = function() ih.set_all() end,
		inlay_hints = { auto = true }
	},

	server = {
		standalone = true,
		on_attach = function(c, b) ih.on_attach(c, b) end
	}
})

local lsp = require("lspconfig")

lsp.zls.setup({})
--lsp.ccls.setup({})
lsp.clangd.setup({})

require("nvim-treesitter.configs").setup({
	highlight = {
		enable = true,
	},
})
