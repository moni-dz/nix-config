''
  compinit -C -d $ZDOTDIR/zcompdump
  export QT_QPA_PLATFORMTHEME=qt5ct
  export PATH=$PATH:$HOME/.config/emacs/bin:$HOME/.config/scripts:/etc/nixos/scripts
  export EXA_ICON_SPACING=2
  export RUSTUP_HOME=~/.local/share/rustup
  export GOPATH=~/Extras/go
  export EDITOR=nvim
  export MANPAGER='nvim +Man!'
  source ~/.config/github_token
  rm .xsession-errors .xsession-errors.old >/dev/null 2>&1
''
