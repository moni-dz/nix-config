''
  syntax on
  set nu
  set guicursor=a:ver25-iCursor
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
  call plug#end()

  au VimEnter * PlugInstall
  colorscheme horizon
  let g:lightline = {'colorscheme' : 'horizon'}
''
