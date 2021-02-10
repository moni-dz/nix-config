''
  syntax on
  set noswapfile
  set nu
  set mouse=a
  set guicursor=a:ver25-iCursor
  set guifont=monospace:h11
  set termguicolors
  set clipboard=unnamedplus

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
    Plug 'vim-syntastic/syntastic'
    Plug 'lervag/vimtex'
    Plug 'elixir-lang/vim-elixir'
    Plug 'jiangmiao/auto-pairs'
    Plug 'gko/vim-coloresque'
    Plug 'sheerun/vim-polyglot'
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
  call plug#end()

  colorscheme horizon
  let g:lightline = { 'colorscheme' : 'horizon' }

  let g:syntastic_mode_map = { "mode": "passive" }

  let g:syntastic_always_populate_loc_list = 1
  let g:syntastic_auto_loc_list = 1
  let g:syntastic_check_on_open = 1
  let g:syntastic_check_on_wq = 1
''
