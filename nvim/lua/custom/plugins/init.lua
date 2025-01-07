return {
  {
    'brenoprata10/nvim-highlight-colors',
    init = function()
      require('nvim-highlight-colors').setup {}
    end,
  },
  {
    'folke/ts-comments.nvim',
    opts = {},
    event = 'VeryLazy',
    enabled = vim.fn.has 'nvim-0.10.0' == 1,
  },
  {
    'folke/zen-mode.nvim',

    opts = {
      window = {
        backdrop = 1,
      },
      plugins = {
        options = {
          enabled = true,
          ruler = false,
          showcmd = false,
          laststatus = 3,
        },
      },
    },
  },
  {
    'romgrk/barbar.nvim',

    init = function()
      vim.g.barbar_auto_setup = false
    end,

    opts = {
      icons = {
        filetype = { enabled = false },
      },
    },
  },
}
