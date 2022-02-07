''
  #!/bin/sh

  specify() { echo "specify either 'nixos' or 'home'" }

  can() {
    echo "will $(tput setaf 3 && tput bold)$1$(tput sgr0) right away..."
    echo "1:$1"
    echo "2:$2"
    case "$1" in
      cd)
        echo "source this file (i.e. $(tput setaf 2). can cd$(tput sgr0)) if it has no effect$(tput sgr0)"
        cd "$HOME/.config/nix-config" || return 1
        ;;
      switch)
        cd ~/.config/nix-config
        case "$2" in
          nixos)
            doas nixos-rebuild switch
            [ "$?" -eq 0 ] && /run/current-system/sw/bin/ls -1 /nix/var/nix/profiles | tail -n 2 | awk '{print "/nix/var/nix/profiles/" $0}' - | xargs nvd diff
            ;;
          home)
            home-manager switch --flake .#fortuneteller2k
            [ "$?" -eq 0 ] && /run/current-system/sw/bin/ls -1 /nix/var/nix/profiles/per-user/fortuneteller2k | tail -n 4 | head -n 2 | awk '{print "/nix/var/nix/profiles/per-user/fortuneteller2k/" $0}' - | xargs nvd diff
            ;;
          *)
            specify
            ;;
        esac
        ;;
      generations)
        case "$2" in
          nixos)
            doas nix-env -p /nix/var/nix/profiles/system --list-generations
            ;;
          home)
            home-manager generations
            ;;
          *)
            specify
            ;;
        esac
        ;;
      test)
        cd ~/.config/nix-config && doas nixos-rebuild test --fast
        ;;
      clean)
        doas nix-collect-garbage -d
        ;;
      upgrade)
        cd ~/.config/nix-config && nix flake update --commit-lock-file --commit-lockfile-summary 'flake: bump flakes'

        case "$2" in
          nixos)
            doas nixos-rebuild switch
            [ "$?" -eq 0 ] && /run/current-system/sw/bin/ls -1 /nix/var/nix/profiles | tail -n 2 | awk '{print "/nix/var/nix/profiles/" $0}' - | xargs nvd diff
            ;;
          home)
            home-manager switch --flake .#fortuneteller2k
            [ "$?" -eq 0 ] && /run/current-system/sw/bin/ls -1 /nix/var/nix/profiles/per-user/fortuneteller2k | tail -n 4 | head -n 2 | awk '{print "/nix/var/nix/profiles/per-user/fortuneteller2k/" $0}' - | xargs nvd diff
            ;;
          *)
            doas nixos-rebuild switch
            [ "$?" -eq 0 ] && /run/current-system/sw/bin/ls -1 /nix/var/nix/profiles | tail -n 2 | awk '{print "/nix/var/nix/profiles/" $0}' - | xargs nvd diff

            home-manager switch --flake .#fortuneteller2k
            [ "$?" -eq 0 ] && /run/current-system/sw/bin/ls -1 /nix/var/nix/profiles/per-user/fortuneteller2k | tail -n 4 | head -n 2 | awk '{print "/nix/var/nix/profiles/per-user/fortuneteller2k/" $0}' - | xargs nvd diff
            ;;
        esac
        [ "$?" -eq 0 ] && /run/current-system/sw/bin/ls -1 /nix/var/nix/profiles/per-user/fortuneteller2k | tail -n 4 | head -n 2 | awk '{print "/nix/var/nix/profiles/per-user/fortuneteller2k/" $0}' - | xargs nvd diff
        ;;
      info)
        cd /etc/nixos || return 1
        nix_str="$(tput setaf 6 && tput bold)Nix$(tput sgr0)"
        nixos_str="$(tput setaf 4 && tput bold)NixOS$(tput sgr0)"

        cat << EOF
  $(tput setaf 3 && tput bold)can$(tput sgr0) is my handy-dandy wrapper script for doing repetitive $nix_str/$nixos_str actions.
   ___________
  /_ _ _ _ _ _\\ $nixos_str version: $(nixos-version)
  |           | $nix_str version: $(nix --version)
  | C   A   N | Last commit: [$(tput setaf 2 && git log -1 --pretty=%h | tr -d '\n' && tput sgr0)] $(git log -1 --pretty=%B)
  |           |
  |_ _ _ _ _ _|
  \\___________/
  EOF
        ;;
      *)
        echo "$(tput setaf 1 && tput bold)no can do, invalid command $1$(tput sgr0)"
        echo 'Usage: can <cd/switch/rollback/generations/test/upgrade/info>'
        return 1;
        ;;
    esac;
  }

  can "$1"
''
