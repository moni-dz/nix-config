''
  syntax on
  set noswapfile
  set nohlsearch
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
    Plug 'junegunn/goyo.vim'
    Plug 'itchyny/lightline.vim'
    Plug 'fortuneteller2k/vim-horizon'
    Plug 'LnL7/vim-nix'
    Plug 'ziglang/zig.vim'
    Plug 'vim-syntastic/syntastic'
    Plug 'lervag/vimtex'
    Plug 'elixir-lang/vim-elixir'
    Plug 'jiangmiao/auto-pairs'
    Plug 'sheerun/vim-polyglot'
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
    Plug 'rrethy/vim-hexokinase', { 'do': 'make hexokinase' }
  call plug#end()

  colorscheme horizon
  let g:lightline = { 'colorscheme' : 'horizon' }

  let g:Hexokinase_highlighters = [ 'backgroundFull' ]

  let g:syntastic_always_populate_loc_list = 1
  let g:syntastic_auto_loc_list = 1
  let g:syntastic_check_on_open = 1
  let g:syntastic_check_on_wq = 1
''
