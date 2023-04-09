{ config, lib, pkgs, system, inputs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = lib.attrValues {
    inherit (pkgs) discord git vim;
    inherit (inputs.agenix.packages.${system}) agenix;
  };

  # Auto upgrade nix package and the daemon service.
  services = {
    nix-daemon.enable = true;
  };

  # Create /etc/zshrc that loads the nix-darwin environment.
  programs = {
    zsh = {
     enable = true;

     interactiveShellInit = ''
       . ${config.age.secrets.github-token.path}
     '';
    }; 
  };

  security.pam.enableSudoTouchIdAuth = true;

  homebrew = {
    enable = true;

    onActivation = {
      autoUpdate = true;
      upgrade = true;
    };

    casks = [
      "amethyst"
      "raycast"
    ];
  };



  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
