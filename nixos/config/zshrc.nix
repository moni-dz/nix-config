''
  function nix() {
    if [[ "$1" == "develop" ]]; then
      command nix develop "$2" -c zsh
    else
      command nix "$@"
    fi
  }

  eval $(dircolors /etc/nixos/nixos/config/LS_COLORS)
  export PATH=$PATH:$HOME/.config/emacs/bin
  export ZDOTDIR=$HOME/.config/zsh
  export HISTFILE=$ZDOTDIR/.zsh_history
  export MANPAGER='nvim +Man!'
''
