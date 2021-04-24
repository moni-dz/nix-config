''
  #!/usr/bin/env dash

  can() {
    echo "will $(tput setaf 3 && tput bold)$1$(tput sgr0) right away..."
    case "$1" in
      cd)
        echo "source this file (i.e. $(tput setaf 2). can cd$(tput sgr0)) if it has no effect$(tput sgr0)"
        cd "$HOME/.config/nix-config" || return 1
        ;;
      switch)
        doas nixos-rebuild switch
        ;;
      test)
        cd ~/.config/nix-config && doas nixos-rebuild test --fast
        ;;
      clean)
        doas nix-collect-garbage -d
        ;;
      upgrade)
        cd ~/.config/nix-config && nix flake update && doas nixos-rebuild switch && git add flake.lock && git commit -m "flake: bump flakes" && git push
        ;;
      info)
        cd ~/.config/nix-config || return 1
        nix_str="$(tput setaf 6 && tput bold)Nix$(tput sgr0)"
        nixos_str="$(tput setaf 4 && tput bold)NixOS$(tput sgr0)"

        cat << EOF
  $(tput setaf 3 && tput bold)can$(tput sgr0) is my handy-dandy wrapper script for doing repetitive $nix_str/$nixos_str actions.
   ___________
  /_ _ _ _ _ _\\ $nixos_str version: $(nixos-version)
  |           | $nix_str version: $(nix --version)
  | C   A   N | Last commit: [$(tput setaf 2 && cd ~/.config/nix-config && git log -1 --pretty=%h | tr -d '\n' && tput sgr0)] $(cd ~/.config/nix-config && git log -1 --pretty=%B)
  |           |
  |_ _ _ _ _ _|
  \\___________/
  EOF
        ;;
      *)
        echo "$(tput setaf 1 && tput bold)no can do, invalid command $1$(tput sgr0)"
        echo 'Usage: can <cd/repl/switch/test/upgrade/info>'
        return 1;
        ;;
    esac;
  }

  can "$1"
''
