return {
  'gpanders/nvim-parinfer',
  'bohlender/vim-smt2',
  {
    'Olical/conjure',
    ft = { 'clojure' },
    lazy = true,

    dependencies = { 'PaterJason/cmp-conjure' },
  },
  {
    'clojure-vim/vim-jack-in',
    dependencies = { 'radenling/vim-dispatch-neovim', 'tpope/vim-dispatch' },
  },
}
