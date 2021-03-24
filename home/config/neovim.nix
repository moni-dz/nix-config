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
    Plug 'glepnir/dashboard-nvim'
    Plug 'junegunn/fzf'
    Plug 'junegunn/goyo.vim'
    Plug 'alvan/vim-closetag'
    Plug 'itchyny/lightline.vim'
    Plug 'fortuneteller2k/vim-flowtune'
    Plug 'nvim-treesitter/nvim-treesitter', { 'do': ':TSUpdate' }
    Plug 'LnL7/vim-nix'
    Plug 'ziglang/zig.vim'
    Plug 'lervag/vimtex'
    Plug 'elixir-lang/vim-elixir'
    Plug 'jiangmiao/auto-pairs'
    Plug 'sheerun/vim-polyglot'
    Plug 'neoclide/coc.nvim', { 'branch': 'release' }
    Plug 'rrethy/vim-hexokinase', { 'do': 'make hexokinase' }
  call plug#end()

  nnoremap <silent> <leader>      :<c-u>WhichKey ' '<CR>
  nnoremap <silent> <localleader> :<c-u>WhichKey ' '<CR>

  colorscheme flowtune
  let g:lightline = { 'colorscheme' : 'flowtune' }
  
  hi Normal ctermbg=none
  hi NonText ctermbg=none
  hi Normal guibg=none
  hi NonText guibg=none

  let g:closetag_filenames = '*.html,*.xhtml,*.phtml,*.xml'
  let g:closetag_xhtml_filenames = '*.xhtml,*.jsx,*.xml'
  let g:closetag_filetypes = 'html,xhtml,phtml,xml'
  let g:closetag_xhtml_filetypes = 'xhtml,jsx,xml'
  let g:closetag_emptyTags_caseSensitive = 1
  let g:closetag_regions = {
      \ 'typescript.tsx': 'jsxRegion,tsxRegion',
      \ 'javascript.jsx': 'jsxRegion',
      \ }
  let g:closetag_shortcut = '>'
''
