''
  syntax on
  set nu
  set mouse=a
  set guicursor=a:ver25-iCursor
  set guifont=monospace:h11
  set termguicolors

  if empty(stdpath('data') . '/plugged')
    silent !curl -fLo "$\{XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
      https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    au VimEnter * PlugInstall
  endif

  call plug#begin(stdpath('data') . '/plugged')
    Plug 'junegunn/fzf'
    Plug 'itchyny/lightline.vim'
    Plug 'ntk148v/vim-horizon'
    Plug 'LnL7/vim-nix'
    Plug 'ziglang/zig.vim'
    Plug 'lervag/vimtex'
    Plug 'elixir-lang/vim-elixir'
    Plug 'tmsvg/pear-tree'
    Plug 'neoclide/coc.nvim', { 'branch': 'release' }
    if has('nvim')
      Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
    else
      Plug 'Shougo/deoplete.nvim'
      Plug 'roxma/nvim-yarp'
      Plug 'roxma/vim-hug-neovim-rpc'
    endif
    Plug 'Shougo/neosnippet.vim'
    Plug 'Shougo/neosnippet-snippets'
  call plug#end()

  colorscheme horizon
  let g:deoplete#enable_at_startup = 1
  let g:lightline = { 'colorscheme' : 'horizon' }
''
