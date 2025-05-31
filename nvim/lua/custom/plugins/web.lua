return {
  {
    'windwp/nvim-ts-autotag',
    config = function()
      require('nvim-ts-autotag').setup {
        opts = {
          -- Defaults
          enable_close = true, -- Auto close tags
          enable_rename = true, -- Auto rename pairs of tags
          enable_close_on_slash = false, -- Auto close on trailing </
        },
      }
    end,
  },
  {
    'davidmh/mdx.nvim',
    event = 'BufEnter *.mdx',
    config = true,
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
  },
  'tpope/vim-surround',
}
