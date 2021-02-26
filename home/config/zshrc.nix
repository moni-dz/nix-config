{ dotDir, home }:

with home;

''
  compinit -C -d ${dotDir}/zcompdump
  export QT_QPA_PLATFORMTHEME=qt5ct
  export PATH=$PATH:${homeDirectory}/.config/emacs/bin:${homeDirectory}/.config/scripts:/etc/nixos/scripts
  export EXA_ICON_SPACING=2
  export RUSTUP_HOME=${homeDirectory}/.local/share/rustup
  export GOPATH=${homeDirectory}/Extras/go
  export EDITOR=nvim
  export MANPAGER='nvim +Man!'
  source ${homeDirectory}/.config/github_token
  rm ${homeDirectory}/.xsession-errors ${homeDirectory}/.xsession-errors.old >/dev/null 2>&1
''
