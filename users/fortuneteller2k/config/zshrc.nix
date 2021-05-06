{ dotDir, home }:

with home;

''
  compinit -C -d ${dotDir}/zcompdump
  export QT_QPA_PLATFORMTHEME=qt5ct
  export RUSTUP_HOME=${homeDirectory}/.local/share/rustup
  export GOPATH=${homeDirectory}/Extras/go
  export EDITOR=nvim
  export MANPAGER='nvim +Man!'
  source ${homeDirectory}/.config/github_token
  rm ${homeDirectory}/.xsession-errors ${homeDirectory}/.xsession-errors.old >/dev/null 2>&1

  gsy() {
    git checkout "$1"
    git fetch upstream
    git merge upstream/"$1"
    git push
  }
''
