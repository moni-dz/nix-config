''
  eval $(dircolors /etc/nixos/nixos/config/LS_COLORS)
  export PATH=$PATH:$HOME/.config/emacs/bin:$HOME/.config/scripts:$HOME/.config/ncmpcpp-ueberzug
  export ZDOTDIR=$HOME/.config/zsh
  export HISTFILE=$ZDOTDIR/.zsh_history
  export MANPAGER='nvim +Man!'
  export GOPATH=$HOME/Extras/go
''
