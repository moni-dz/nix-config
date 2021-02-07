''
  eval $(dircolors /etc/nixos/nixos/config/LS_COLORS)
  compinit -C -d i$ZDOTDIR/zcompdump
  export HISTFILE=$ZDOTDIR/zsh_history
  export QT_QPA_PLATFORMTHEME=qt5ct
  export _JAVA_AWT_WM_NONREPARENTING=1
  export RUSTUP_HOME=$HOME/.local/share/rustup
  export PATH=$PATH:$HOME/.config/emacs/bin:$HOME/.config/scripts
  export EDITOR=nvim
  export MANPAGER='nvim +Man!'
  export GOPATH=$HOME/Extras/go
''
