''
  eval $(dircolors /etc/nixos/config/LS_COLORS)
  compinit -C -d i$ZDOTDIR/zcompdump
  export HISTFILE=$ZDOTDIR/zsh_history
  export QT_QPA_PLATFORMTHEME=qt5ct
  export RUSTUP_HOME=$HOME/.local/share/rustup
  export PATH=$PATH:$HOME/.config/emacs/bin:$HOME/.config/scripts:/etc/nixos/scripts
  export EDITOR=nvim
  export MANPAGER='nvim +Man!'
  export GOPATH=$HOME/Extras/go
  export EXA_ICON_SPACING=2
  source ~/.config/github_token
''
